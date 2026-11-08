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

/** Something that can be sent to Redis, replying with an `A`. It's covariant, so that a later version of a command can reply with more */
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

  /** A single Redis command */
  sealed trait Single[+A] extends Protocol[A] { self =>
    def command: String
    def parameters: Seq[GenBulk]
    def decode(resp: RESP): Maybe[A]

    final def encode: RESP = Arr(Bulk(command), parameters*)

    override private[laserdisc] final def frames: Seq[RESP]                           = Seq(encode)
    override private[laserdisc] final def decodeReplies(replies: Seq[RESP]): Maybe[A] = decode(replies.head)

    override final def map[B](f: A => B): Single[B] =
      new Single[B] {
        override def command: String              = self.command
        override def parameters: Seq[GenBulk]     = self.parameters
        override def decode(resp: RESP): Maybe[B] = self.decode(resp).map(f(_))
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

    /** We request evidence that the [[RESP]] sent back by Redis can be deserialized into an instance of a B.
      *
      * Ensuring this is the case here guarantees that no instance of a [[Protocol]] can '''ever''' be constructed unless proven that the
      * response we eventually will get back can be deserialized into the expected type B.
      *
      * @tparam A
      *   The [[RESP]] type of the response expected from Redis
      * @tparam B
      *   The type we expect to convert an A into
      *
      * @return
      *   A [[Single]] command, sending this command and parameters and reading the reply as a B
      */
    final def as[A, B](implicit R: RESPRead.Aux[A, B]): Single[B] = using(R)

    final def using[B](R: RESPRead[B]): Single[B] =
      new Single[B] {
        override final val command: String              = self.command
        override final val parameters: Seq[GenBulk]     = RESPParamWrite[L].write(l)
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

  /** The way to create a [[Multi]] transaction for the commands in `in`. EXEC's reply is "null" when a WATCHed key changed and nothing ran,
    * which is read as `None`, otherwise it holds one result per command.
    */
  final def multi[In <: Tuple, Out <: Tuple](in: In)(implicit M: MultiResults.Aux[In, Out]): Multi[Option[Out]] =
    new Multi[Option[Out]] {
      override final val protocols: List[Single[Any]] = M.protocols(in)

      private def decodeExec(resp: RESP): Maybe[Option[Out]] = resp match {
        case Arr(results) if results.size == protocols.size => Right(Some(M.read(in, results)))
        case NilArr                                         => Right(None)
        case err: Err                                       => Left(err)
        case other => Left(RESPDecErr(s"Expected the results of ${protocols.size} commands but got: $other"))
      }

      override private[laserdisc] final val frames: Seq[RESP] = (Arr(Bulk("MULTI")) :: protocols.map(_.encode)) :+ Arr(Bulk("EXEC"))

      // when Redis rejects a command, EXEC fails without saying why: the rejection is more useful
      override private[laserdisc] final def decodeReplies(replies: Seq[RESP]): Maybe[Option[Out]] =
        replies.init.collectFirst { case err: Err => err }.fold(decodeExec(replies.last))(Left(_))
    }

  /** The way to create a [[Single]] command, through this partial application (see [[multi]] for a transaction).
    *
    * This apply method requires the caller to provide the type of request parameters L this [[Protocol]] expects to deal with when encoding
    * the request parameters into a [[RESP]] [[GenArr]] instance to send to Redis.
    */
  final def apply[L: RESPParamWrite](cmd: String, l: L): PartiallyAppliedProtocol[L] = new PartiallyAppliedProtocol(cmd, l) {}
}
