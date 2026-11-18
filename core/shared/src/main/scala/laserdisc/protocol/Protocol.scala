/*
 * Copyright (c) 2018-2026 LaserDisc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package laserdisc
package protocol

/** A command, or a transaction of commands, to send with a client, whose reply is read as an `A`. A version's import, such as
  * `laserdisc.redis_8_10`, has its commands
  */
// covariant, so that a later version of a command can reply with more
sealed trait Protocol[+A] {

  /** What is sent to Redis, which replies to each one */
  private[laserdisc] def frames: Seq[RESP]

  /** Reads the replies to [[frames]], one for each */
  private[laserdisc] def decodeReplies(replies: Seq[RESP]): Maybe[A]

  def map[B](f: A => B): Protocol[B]
}

object Protocol {

  // the lines of text a command's HELP subcommand replies with
  private[protocol] final def help(command: String): Single[Seq[String]] = Protocol(command, "HELP").as[Arr.Of[Str], Seq[String]]

  /** A single command: its name, its parameters and how its reply is read */
  sealed trait Single[+A] extends Protocol[A] { self =>
    def command: String
    def parameters: Seq[Bulk]
    def decode(resp: RESP): Maybe[A]

    final def encode: RESP = Arr(Bulk(command), parameters*)

    override private[laserdisc] final def frames: Seq[RESP]                           = Seq(encode)
    override private[laserdisc] final def decodeReplies(replies: Seq[RESP]): Maybe[A] = decode(replies.head)

    override final def map[B](f: A => B): Single[B] =
      new Single[B] {
        override def command: String              = self.command
        override def parameters: Seq[Bulk]        = self.parameters
        override def decode(resp: RESP): Maybe[B] = self.decode(resp).map(f(_))
      }

    // the same command, with the parameter at index set to value
    private[protocol] final def withParameter(index: Int, value: String): Single[A] =
      new Single[A] {
        override val command: String              = self.command
        override val parameters: Seq[Bulk]        = self.parameters.updated(index, Bulk(value))
        override def decode(resp: RESP): Maybe[A] = self.decode(resp)
      }

    override final def toString: String = s"Protocol($command)"
  }

  /** A transaction: MULTI, then its [[Single]] commands, then EXEC. Redis replies QUEUED to each command, and EXEC's reply holds their
    * results
    */
  sealed trait Multi[+A] extends Protocol[A] { self =>
    def protocols: List[Single[Any]]

    override final def map[B](f: A => B): Multi[B] =
      new Multi[B] {
        override def protocols: List[Single[Any]] = self.protocols

        override private[laserdisc] def frames: Seq[RESP]                           = self.frames
        override private[laserdisc] def decodeReplies(replies: Seq[RESP]): Maybe[B] = self.decodeReplies(replies).map(f(_))
      }

    override final def toString: String = s"Protocol(MULTI ${protocols.map(_.command).mkString(" ")} EXEC)"
  }

  sealed abstract class PartiallyAppliedProtocol[L: RESPParamWrite](
      private final val command: String,
      private final val l: L
  ) { self =>

    /** The command, with its reply read as a `B`: a command can only be made for a reply there's a way to read.
      *
      * @tparam A
      *   The kind of reply the server sends, such as [[Bulk]] or [[Num]]
      * @tparam B
      *   The type to read it as
      */
    final def as[A, B](implicit R: RESPRead.Aux[A, B]): Single[B] = using(R)

    final def using[B](R: RESPRead[B]): Single[B] =
      new Single[B] {
        override final val command: String              = self.command
        override final val parameters: Seq[Bulk]        = RESPParamWrite[L].write(l)
        override final def decode(resp: RESP): Maybe[B] = R.read(resp)
      }
  }

  /** Reads the results of a tuple of [[Single]] commands, one for each, into a tuple of the same size */
  sealed trait MultiResults[In <: Tuple] {
    type Out <: Tuple

    def protocols(in: In): List[Single[Any]]
    def read(in: In, results: List[RESP]): Out
  }

  object MultiResults {
    final type Aux[In <: Tuple, Out0 <: Tuple] = MultiResults[In] { type Out = Out0 }

    implicit final val emptyMultiResults: MultiResults.Aux[EmptyTuple, EmptyTuple] =
      new MultiResults[EmptyTuple] {
        override final type Out = EmptyTuple
        override final def protocols(in: EmptyTuple): List[Single[Any]]          = Nil
        override final def read(in: EmptyTuple, results: List[RESP]): EmptyTuple = EmptyTuple
      }

    implicit final def consMultiResults[A, T <: Tuple, TOut <: Tuple](
        implicit T: MultiResults.Aux[T, TOut]
    ): MultiResults.Aux[Single[A] *: T, Maybe[A] *: TOut] =
      new MultiResults[Single[A] *: T] {
        override final type Out = Maybe[A] *: TOut
        override final def protocols(in: Single[A] *: T): List[Single[Any]] =
          in match { case protocol *: rest => protocol :: T.protocols(rest) }
        override final def read(in: Single[A] *: T, results: List[RESP]): Maybe[A] *: TOut =
          in match { case protocol *: rest => protocol.decode(results.head) *: T.read(rest, results.tail) }
      }
  }

  /** A transaction of the commands in `in`, a tuple of one or more [[Single]] commands: MULTI, the commands, then EXEC. It's read as `None`
    * when a watched key changed and nothing ran, and otherwise as a tuple of each command's result.
    */
  final def multi[In <: NonEmptyTuple, Out <: Tuple](in: In)(implicit M: MultiResults.Aux[In, Out]): Multi[Option[Out]] =
    new Multi[Option[Out]] {
      override final val protocols: List[Single[Any]] = M.protocols(in)

      private def decodeExec(resp: RESP): Maybe[Option[Out]] = resp match {
        case Arr(results) if results.size == protocols.size => Right(Some(M.read(in, results)))
        case NilArr                                         => Right(None)
        case err: Err                                       => Left(err)
        case other => Left(RESPDecErr(s"Expected the results of ${protocols.size} commands but got: ${RESPDecErr.shown(other)}"))
      }

      override private[laserdisc] final val frames: Seq[RESP] = (Arr(Bulk("MULTI")) :: protocols.map(_.encode)) :+ Arr(Bulk("EXEC"))

      // when Redis rejects a command, EXEC fails without saying why: the rejection is more useful
      override private[laserdisc] final def decodeReplies(replies: Seq[RESP]): Maybe[Option[Out]] =
        replies.init.collectFirst { case err: Err => err }.fold(decodeExec(replies.last))(Left(_))
    }

  /** Starts a command of your own, named `cmd`, with the parameters `l`, which a [[RESPParamWrite]] writes as the strings the server gets.
    * Finish it with `as[A, B]`, to read its reply as a `B`, or with `using`. For a transaction, see [[multi]].
    *
    * {{{
    * val echo: Protocol[String] = Protocol("ECHO", "hello").as[Bulk, String]
    * }}}
    */
  final def apply[L: RESPParamWrite](cmd: String, l: L): PartiallyAppliedProtocol[L] = new PartiallyAppliedProtocol(cmd, l) {}
}
