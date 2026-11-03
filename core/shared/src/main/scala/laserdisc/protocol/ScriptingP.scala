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

import Protocol.Single

object ScriptingP {

  /** REPLACE, for FUNCTION LOAD: replace the library if one of the same name is loaded */
  sealed trait Replace
  object Replace extends Replace {
    implicit val replaceShow: Show[Replace] = Show.const("REPLACE")
  }

  /** WITHCODE, for FUNCTION LIST: also give each library's code */
  sealed trait WithCode
  object WithCode extends WithCode {
    implicit val withCodeShow: Show[WithCode] = Show.const("WITHCODE")
  }

  /** A function of a library: its name, its description, if it has one, and its flags (like `no-writes`) */
  final case class Function(name: Key, description: Option[String], flags: Seq[String])

  /** A library of FUNCTION LIST: its name, its engine (like `LUA`), its functions, and its code when asked for WITHCODE */
  final case class Library(name: Key, engine: String, functions: Seq[Function], code: Option[String])

  /** The function FUNCTION STATS found running: its name, the command that called it, and for how long it's been running */
  final case class Running(name: Key, command: Seq[String], durationMilliseconds: NonNegLong)

  /** How many libraries and functions an engine has loaded */
  final case class Engine(name: String, libraries: NonNegLong, functions: NonNegLong)

  /** FUNCTION STATS: the function running, if any, and each engine's libraries and functions */
  final case class Stats(running: Option[Running], engines: Seq[Engine])

  private[protocol] final val sha1: Read[Bulk, Sha1] = Read.instancePF("Bulk(Sha1)") { case Bulk(Sha1(sha)) => sha }

  private final def byName(fields: Seq[RESP]): Map[String, RESP] = fields.grouped(2).collect { case Seq(Bulk(k), v) => k -> v }.toMap

  // FUNCTION LIST's flags are simple strings
  private final def texts(resp: RESP): Option[Seq[String]] = resp match {
    case Arr(elements) => Some(elements.collect { case Bulk(s) => s; case Str(s) => s }).filter(_.size == elements.size)
    case NilArr        => Some(Nil)
    case _             => None
  }

  private final def number(resp: Option[RESP]): Option[NonNegLong] = resp.collect { case Num(NonNegLong(n)) => n }

  private final def function(resp: RESP): Option[Function] = resp match {
    case Arr(fields) =>
      val m = byName(fields)
      for {
        name        <- m.get("name").collect { case Bulk(Key(k)) => k }
        description <- m.get("description").collect { case Bulk(d) => Some(d); case NullBulk => None }
        flags       <- m.get("flags").flatMap(texts)
      } yield Function(name, description, flags)
    case _ => None
  }

  private final def library(resp: RESP): Option[Library] = resp match {
    case Arr(fields) =>
      val m = byName(fields)
      for {
        name      <- m.get("library_name").collect { case Bulk(Key(k)) => k }
        engine    <- m.get("engine").collect { case Bulk(e) => e }
        functions <- m.get("functions").collect { case Arr(fs) => fs.map(function) }.filter(_.forall(_.isDefined))
      } yield Library(name, engine, functions.flatten, m.get("library_code").collect { case Bulk(code) => code })
    case _ => None
  }

  private[protocol] final val libraries: Read[Arr, Seq[Library]] = Read.instance { case Arr(elements) =>
    val read = elements.map(library)
    if (read.forall(_.isDefined)) Right(read.flatten) else Left(RESPDecErr(s"Unexpected function libraries. Was $elements"))
  }

  private final def running(resp: RESP): Option[Option[Running]] = resp match {
    case NullBulk | NilArr => Some(None)
    case Arr(fields)       =>
      val m = byName(fields)
      for {
        name     <- m.get("name").collect { case Bulk(Key(k)) => k }
        command  <- m.get("command").flatMap(texts)
        duration <- number(m.get("duration_ms"))
      } yield Some(Running(name, command, duration))
    case _ => None
  }

  private final def engines(resp: RESP): Option[Seq[Engine]] = resp match {
    case Arr(fields) =>
      val read = fields.grouped(2).toList.map {
        case Seq(Bulk(name), Arr(counts)) =>
          val m = byName(counts)
          for {
            libraries <- number(m.get("libraries_count"))
            functions <- number(m.get("functions_count"))
          } yield Engine(name, libraries, functions)
        case _ => None
      }
      if (read.forall(_.isDefined)) Some(read.flatten) else None
    case _ => None
  }

  private[protocol] final val stats: Read[Arr, Stats] = Read.instance { case Arr(fields) =>
    val m = byName(fields)
    (for {
      r  <- m.get("running_script").flatMap(running)
      es <- m.get("engines").flatMap(engines)
    } yield Stats(r, es)).toRight(RESPDecErr(s"Unexpected function stats. Was $fields"))
  }

  // the number of keys, the keys, then the arguments
  private[protocol] final def keysAndArgs(keys: List[Key], args: List[Value]): List[String] =
    keys.size.toString :: keys.map(_.value) ++ args.map(_.shown)
}

/** SCRIPT, under `script`. Redis 6.2 */
trait ScriptCommands extends Features {
  import ClusterManagement.FlushMode

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("SCRIPT EXISTS", "SCRIPT FLUSH: ASYNC SYNC", "SCRIPT KILL", "SCRIPT LOAD")

  /** Whether each script is in the cache */
  def exists(shas: OneOrMore[Sha1]): Single.Aux[Seq[Boolean]] =
    Protocol("SCRIPT", "EXISTS" :: shas.value.map(_.value)).as[Arr.Of[Num], Seq[Boolean]]

  /** Empties the script cache */
  val flush: Single.Aux[OK]                  = Protocol("SCRIPT", "FLUSH").as[Str, OK]
  def flush(mode: FlushMode): Single.Aux[OK] = Protocol("SCRIPT", "FLUSH" *: mode *: EmptyTuple).as[Str, OK]

  /** Stops the script running, if it hasn't written anything */
  val kill: Single.Aux[OK] = Protocol("SCRIPT", "KILL").as[Str, OK]

  /** Puts the script in the cache, without running it, and replies with its SHA1 digest, for EVALSHA */
  def load(script: String): Single.Aux[Sha1] = Protocol("SCRIPT", "LOAD" :: script :: Nil).using(RESPRead.instance(ScriptingP.sha1))
}

/** SCRIPT SHOW, under `script`: Valkey 8.0 */
trait ScriptShowCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SCRIPT SHOW")

  /** The source of a script in the cache */
  def show(sha: Sha1): Single.Aux[String] = Protocol("SCRIPT", "SHOW" :: sha.value :: Nil).as[Bulk, String]
}

/** EVAL and EVALSHA, and SCRIPT under `script`. Redis 6.2
  *
  * A script replies with whatever it returns, so you say how to read it, as with `eval[Num, Long]`. The keys are KEYS in the script and the
  * arguments ARGV
  */
trait ScriptingBaseP extends Features {
  import ScriptingP.keysAndArgs

  override private[laserdisc] def supported: List[String] = super.supported ++ List("EVAL", "EVALSHA") ++ script.supported

  object scriptingtypes {
    final type ScriptingEngine    = ScriptingP.Engine
    final type ScriptingFlushMode = ClusterManagement.FlushMode
    final type ScriptingFunction  = ScriptingP.Function
    final type ScriptingLibrary   = ScriptingP.Library
    final type ScriptingReplace   = ScriptingP.Replace
    final type ScriptingRunning   = ScriptingP.Running
    final type ScriptingStats     = ScriptingP.Stats
    final type ScriptingWithCode  = ScriptingP.WithCode

    final val ScriptingEngine    = ScriptingP.Engine
    final val ScriptingFlushMode = ClusterManagement.FlushMode
    final val ScriptingFunction  = ScriptingP.Function
    final val ScriptingLibrary   = ScriptingP.Library
    final val ScriptingReplace   = ScriptingP.Replace
    final val ScriptingRunning   = ScriptingP.Running
    final val ScriptingStats     = ScriptingP.Stats
    final val ScriptingWithCode  = ScriptingP.WithCode
  }

  lazy val script: ScriptCommands = new ScriptCommands {}

  private[protocol] final def run[R, A](command: String, first: String, keys: List[Key], args: List[Value])(
      R: RESPRead.Aux[R, A]
  ): Single.Aux[A] = Protocol(command, first :: keysAndArgs(keys, args)).using(R)

  final def eval[R, A](script: String)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] = run("EVAL", script, Nil, Nil)(R)
  final def eval[R, A](script: String, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVAL", script, keys.value, Nil)(R)
  final def evalargs[R, A](script: String, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVAL", script, Nil, args.value)(R)
  final def eval[R, A](script: String, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVAL", script, keys.value, args.value)(R)

  /** EVAL of a script in the cache, by its SHA1 digest */
  final def evalsha[R, A](sha: Sha1)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] = run("EVALSHA", sha.value, Nil, Nil)(R)
  final def evalsha[R, A](sha: Sha1, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVALSHA", sha.value, keys.value, Nil)(R)
  final def evalshaargs[R, A](sha: Sha1, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVALSHA", sha.value, Nil, args.value)(R)
  final def evalsha[R, A](sha: Sha1, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVALSHA", sha.value, keys.value, args.value)(R)
}

/** FUNCTION, under `function`. Redis 7.0. Not DUMP and RESTORE yet: their payload is binary, which a [[Bulk]] doesn't keep intact */
trait FunctionCommands extends Features {
  import ClusterManagement.FlushMode
  import ScriptingP.{Library, Replace, Stats, WithCode}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "FUNCTION DELETE",
      "FUNCTION FLUSH: ASYNC SYNC",
      "FUNCTION KILL",
      "FUNCTION LIST: LIBRARYNAME WITHCODE",
      "FUNCTION LOAD: REPLACE",
      "FUNCTION STATS"
    )

  /** Deletes the library and its functions */
  def delete(library: Key): Single.Aux[OK] = Protocol("FUNCTION", "DELETE" :: library.value :: Nil).as[Str, OK]

  /** Deletes every library */
  val flush: Single.Aux[OK]                  = Protocol("FUNCTION", "FLUSH").as[Str, OK]
  def flush(mode: FlushMode): Single.Aux[OK] = Protocol("FUNCTION", "FLUSH" *: mode *: EmptyTuple).as[Str, OK]

  /** Stops the function running, if it hasn't written anything */
  val kill: Single.Aux[OK] = Protocol("FUNCTION", "KILL").as[Str, OK]

  private final val libraries = RESPRead.instance(ScriptingP.libraries)

  /** The libraries, or those whose name matches the pattern */
  val list: Single.Aux[Seq[Library]]                       = Protocol("FUNCTION", "LIST").using(libraries)
  def list(pattern: GlobPattern): Single.Aux[Seq[Library]] =
    Protocol("FUNCTION", "LIST" *: "LIBRARYNAME" *: pattern *: EmptyTuple).using(libraries)
  def list(withCode: WithCode): Single.Aux[Seq[Library]] = Protocol("FUNCTION", "LIST" *: withCode *: EmptyTuple).using(libraries)
  def list(pattern: GlobPattern, withCode: WithCode): Single.Aux[Seq[Library]] =
    Protocol("FUNCTION", "LIST" *: "LIBRARYNAME" *: pattern *: withCode *: EmptyTuple).using(libraries)

  /** Loads a library from its code, which starts with `#!lua name=<library>`, and replies with the library's name */
  def load(code: String): Single.Aux[Key]                   = Protocol("FUNCTION", "LOAD" :: code :: Nil).as[Bulk, Key]
  def load(code: String, replace: Replace): Single.Aux[Key] = Protocol("FUNCTION", "LOAD" *: replace *: code *: EmptyTuple).as[Bulk, Key]

  val stats: Single.Aux[Stats] = Protocol("FUNCTION", "STATS").using(RESPRead.instance(ScriptingP.stats))
}

/** EVAL_RO, EVALSHA_RO, FCALL and FCALL_RO, and FUNCTION under `function`. Redis 7.0 */
trait ScriptingFunctionsP extends ScriptingBaseP {
  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("EVAL_RO", "EVALSHA_RO", "FCALL", "FCALL_RO") ++ function.supported

  lazy val function: FunctionCommands = new FunctionCommands {}

  /** EVAL of a script that only reads, which may run on a replica */
  final def evalro[R, A](script: String)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] = run("EVAL_RO", script, Nil, Nil)(R)
  final def evalro[R, A](script: String, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVAL_RO", script, keys.value, Nil)(R)
  final def evalroargs[R, A](script: String, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVAL_RO", script, Nil, args.value)(R)
  final def evalro[R, A](script: String, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVAL_RO", script, keys.value, args.value)(R)

  /** EVALSHA of a script that only reads, which may run on a replica */
  final def evalsharo[R, A](sha: Sha1)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] = run("EVALSHA_RO", sha.value, Nil, Nil)(R)
  final def evalsharo[R, A](sha: Sha1, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVALSHA_RO", sha.value, keys.value, Nil)(R)
  final def evalsharoargs[R, A](sha: Sha1, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVALSHA_RO", sha.value, Nil, args.value)(R)
  final def evalsharo[R, A](sha: Sha1, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("EVALSHA_RO", sha.value, keys.value, args.value)(R)

  /** Calls a function of a loaded library */
  final def fcall[R, A](function: Key)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] = run("FCALL", function.value, Nil, Nil)(R)
  final def fcall[R, A](function: Key, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("FCALL", function.value, keys.value, Nil)(R)
  final def fcallargs[R, A](function: Key, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("FCALL", function.value, Nil, args.value)(R)
  final def fcall[R, A](function: Key, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("FCALL", function.value, keys.value, args.value)(R)

  /** FCALL of a function flagged `no-writes`, which may run on a replica */
  final def fcallro[R, A](function: Key)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] = run("FCALL_RO", function.value, Nil, Nil)(R)
  final def fcallro[R, A](function: Key, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("FCALL_RO", function.value, keys.value, Nil)(R)
  final def fcallroargs[R, A](function: Key, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("FCALL_RO", function.value, Nil, args.value)(R)
  final def fcallro[R, A](function: Key, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single.Aux[A] =
    run("FCALL_RO", function.value, keys.value, args.value)(R)
}

trait ScriptAllCommands extends ScriptCommands with ScriptShowCommands

trait ScriptingP extends ScriptingFunctionsP {
  override lazy val script: ScriptAllCommands = new ScriptAllCommands {}
}
