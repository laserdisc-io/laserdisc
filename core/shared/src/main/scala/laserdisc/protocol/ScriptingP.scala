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

  /** What FUNCTION RESTORE does with the libraries already loaded: `append` to them, failing if a name clashes, which is the default,
    * `flush` them first, or `replace` those of the same name
    */
  sealed trait RestorePolicy
  object RestorePolicy {
    case object append  extends RestorePolicy
    case object flush   extends RestorePolicy
    case object replace extends RestorePolicy

    implicit val restorePolicyShow: Show[RestorePolicy] = Show.instance {
      case `append`  => "APPEND"
      case `flush`   => "FLUSH"
      case `replace` => "REPLACE"
    }
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

  private implicit final val function: Read[Record, Function] = Read.record("function") { f =>
    for {
      name        <- f.bulk[Key]("name")
      description <- f[OrNullBulk[Bulk], Option[String]]("description")
      flags       <- f[Arr.Of[Str], Seq[String]]("flags")
    } yield Function(name, description, flags)
  }

  private implicit final val library: Read[Record, Library] = Read.record("function library") { f =>
    for {
      name      <- f.bulk[Key]("library_name")
      engine    <- f.bulk[String]("engine")
      functions <- f[Records, Seq[Function]]("functions")
      code      <- f.optionalBulk[String]("library_code")
    } yield Library(name, engine, functions, code)
  }
  private[protocol] final val libraries: Read[Records, Seq[Library]] = Read[Records, Seq[Library]]

  private implicit final val running: Read[Record, Running] = Read.record("running function") { f =>
    for {
      name     <- f.bulk[Key]("name")
      command  <- f[Texts, Seq[String]]("command")
      duration <- f.num[NonNegLong]("duration_ms")
    } yield Running(name, command, duration)
  }

  // how many libraries and functions an engine has loaded, under its name
  private implicit final val counts: Read[Record, (NonNegLong, NonNegLong)] = Read.record("engine") { f =>
    for {
      libraries <- f.num[NonNegLong]("libraries_count")
      functions <- f.num[NonNegLong]("functions_count")
    } yield libraries -> functions
  }

  private[protocol] final val stats: Read[Record, Stats] = Read.record("function stats") { f =>
    for {
      running <- f[OrNullBulk[Record], Option[Running]]("running_script")
      engines <- f[Arr.OfPairs[Bulk, Record], Seq[(String, (NonNegLong, NonNegLong))]]("engines")
    } yield Stats(running, engines.map { case (name, (libraries, functions)) => Engine(name, libraries, functions) })
  }

  // the number of keys, the keys, then the arguments
  private[protocol] final def keysAndArgs(keys: List[Key], args: List[Value]): List[String] =
    keys.size.toString :: keys.map(_.value) ++ args.map(_.shown)
}

/** SCRIPT, under `script`. Redis 6.2 */
trait ScriptCommands extends Features {
  import ClusterManagement.FlushMode

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("SCRIPT EXISTS", "SCRIPT FLUSH: ASYNC SYNC", "SCRIPT HELP", "SCRIPT KILL", "SCRIPT LOAD")

  val help: Single.Aux[Seq[String]] = Protocol.help("SCRIPT")

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
    final type ScriptingEngine        = ScriptingP.Engine
    final type ScriptingFlushMode     = ClusterManagement.FlushMode
    final type ScriptingFunction      = ScriptingP.Function
    final type ScriptingLibrary       = ScriptingP.Library
    final type ScriptingReplace       = ScriptingP.Replace
    final type ScriptingRestorePolicy = ScriptingP.RestorePolicy
    final type ScriptingRunning       = ScriptingP.Running
    final type ScriptingStats         = ScriptingP.Stats
    final type ScriptingWithCode      = ScriptingP.WithCode

    final val ScriptingEngine        = ScriptingP.Engine
    final val ScriptingFlushMode     = ClusterManagement.FlushMode
    final val ScriptingFunction      = ScriptingP.Function
    final val ScriptingLibrary       = ScriptingP.Library
    final val ScriptingReplace       = ScriptingP.Replace
    final val ScriptingRestorePolicy = ScriptingP.RestorePolicy
    final val ScriptingRunning       = ScriptingP.Running
    final val ScriptingStats         = ScriptingP.Stats
    final val ScriptingWithCode      = ScriptingP.WithCode
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

/** FUNCTION, under `function`. Redis 7.0 */
trait FunctionCommands extends Features {
  import ClusterManagement.FlushMode
  import ScriptingP.{Library, Replace, RestorePolicy, Stats, WithCode}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "FUNCTION DELETE",
      "FUNCTION DUMP",
      "FUNCTION FLUSH: ASYNC SYNC",
      "FUNCTION HELP",
      "FUNCTION KILL",
      "FUNCTION LIST: LIBRARYNAME WITHCODE",
      "FUNCTION LOAD: REPLACE",
      "FUNCTION RESTORE: APPEND FLUSH REPLACE",
      "FUNCTION STATS"
    )

  val help: Single.Aux[Seq[String]] = Protocol.help("FUNCTION")

  /** Deletes the library and its functions */
  def delete(library: Key): Single.Aux[OK] = Protocol("FUNCTION", "DELETE" :: library.value :: Nil).as[Str, OK]

  /** Every library, in a binary form for `restore` */
  val dump: Single.Aux[Bulk] = Protocol("FUNCTION", "DUMP").as[Bulk, Bulk]

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

  /** Loads the libraries of a `dump` */
  def restore(payload: Bulk): Single.Aux[OK]                        = Protocol("FUNCTION", "RESTORE" *: payload *: EmptyTuple).as[Str, OK]
  def restore(payload: Bulk, policy: RestorePolicy): Single.Aux[OK] =
    Protocol("FUNCTION", "RESTORE" *: payload *: policy *: EmptyTuple).as[Str, OK]

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
