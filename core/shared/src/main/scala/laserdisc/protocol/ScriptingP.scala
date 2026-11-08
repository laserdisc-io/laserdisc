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
  private[laserdisc] object Replace extends Replace {
    implicit val replaceShow: Show[Replace] = Show.const("REPLACE")
  }

  /** WITHCODE, for FUNCTION LIST: also give each library's code */
  sealed trait WithCode
  private[laserdisc] object WithCode extends WithCode {
    implicit val withCodeShow: Show[WithCode] = Show.const("WITHCODE")
  }

  /** What FUNCTION RESTORE does with the libraries already loaded: `append` to them, failing if a name clashes, which is the default,
    * `flush` them first, or `replace` those of the same name
    */
  sealed trait RestorePolicy
  trait RestorePolicyBuilders {
    val append: RestorePolicy
    val flush: RestorePolicy
    val replace: RestorePolicy
  }
  private[laserdisc] object RestorePolicy extends RestorePolicyBuilders {
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
    Show[Int].show(keys.size) :: keys.map(_.value) ++ args.map(_.shown)

  // the builders each feature's commands take, and the types of their replies
  trait ScriptingTypes {
    final type ScriptingFlushMode = ClusterManagement.FlushMode
    final val ScriptingFlushMode = ClusterManagement.FlushMode
  }
  trait ScriptingFunctionTypes {
    final type ScriptingEngine        = ScriptingP.Engine
    final type ScriptingFunction      = ScriptingP.Function
    final type ScriptingLibrary       = ScriptingP.Library
    final type ScriptingReplace       = ScriptingP.Replace
    final type ScriptingRestorePolicy = ScriptingP.RestorePolicy
    final type ScriptingRunning       = ScriptingP.Running
    final type ScriptingStats         = ScriptingP.Stats
    final type ScriptingWithCode      = ScriptingP.WithCode
    final val ScriptingEngine                                          = ScriptingP.Engine
    final val ScriptingFunction                                        = ScriptingP.Function
    final val ScriptingLibrary                                         = ScriptingP.Library
    final val ScriptingReplace: ScriptingP.Replace                     = ScriptingP.Replace
    final val ScriptingRestorePolicy: ScriptingP.RestorePolicyBuilders = ScriptingP.RestorePolicy
    final val ScriptingRunning                                         = ScriptingP.Running
    final val ScriptingStats                                           = ScriptingP.Stats
    final val ScriptingWithCode: ScriptingP.WithCode                   = ScriptingP.WithCode
  }

  // the same names for laserdisc's own code, which can't read the members a version overrides while it's being built
  private[protocol] object internal extends ScriptingTypes with ScriptingFunctionTypes
}

/** SCRIPT, under `script`. Redis 6.2 */
trait ScriptCoreCommands extends Features {
  import ClusterManagement.FlushMode

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List("SCRIPT EXISTS", "SCRIPT FLUSH: ASYNC SYNC", "SCRIPT HELP", "SCRIPT LOAD")

  val help: Single[Seq[String]] = Protocol.help("SCRIPT")

  /** Whether each script is in the cache */
  def exists(shas: OneOrMore[Sha1]): Single[Seq[Boolean]] =
    Protocol("SCRIPT", "EXISTS" :: shas.value.map(_.value)).as[Arr.Of[Num], Seq[Boolean]]
  def exists(sha: Sha1, shas: Sha1*): Single[Seq[Boolean]] =
    exists(OneOrMore.unsafeFrom(sha :: shas.toList))

  /** Empties the script cache */
  val flush: Single[OK]                  = Protocol("SCRIPT", "FLUSH").as[Str, OK]
  def flush(mode: FlushMode): Single[OK] = Protocol("SCRIPT", "FLUSH" *: mode *: EmptyTuple).as[Str, OK]

  /** Puts the script in the cache, without running it, and replies with its SHA1 digest, for EVALSHA */
  def load(script: String): Single[Sha1] = Protocol("SCRIPT", "LOAD" :: script :: Nil).using(RESPRead.instance(ScriptingP.sha1))
}

/** SCRIPT with KILL, under `script`. Redis 6.2 */
trait ScriptCommands extends ScriptCoreCommands {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SCRIPT KILL")

  /** Stops the script running, if it hasn't written anything */
  val kill: Single[OK] = Protocol("SCRIPT", "KILL").as[Str, OK]
}

/** SCRIPT SHOW, under `script`: Valkey 8.0 */
trait ScriptShowCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("SCRIPT SHOW")

  /** The source of a script in the cache */
  def show(sha: Sha1): Single[String] = Protocol("SCRIPT", "SHOW" :: sha.value :: Nil).as[Bulk, String]
}

/** EVAL and EVALSHA, and SCRIPT under `script`. Redis 6.2
  *
  * A script replies with whatever it returns, so you say how to read it, as with `eval[Num, Long]`. The keys are KEYS in the script and the
  * arguments ARGV
  */
trait ScriptingCoreP extends Features {
  import ScriptingP.keysAndArgs

  override private[laserdisc] def supported: List[String] = super.supported ++ List("EVAL", "EVALSHA")

  /** The builders of this family's commands, and the types of their replies */
  val scriptingtypes: ScriptingP.ScriptingTypes = new ScriptingP.ScriptingTypes {}

  private[protocol] final def run[R, A](command: String, first: String, keys: List[Key], args: List[Value])(
      R: RESPRead.Aux[R, A]
  ): Single[A] = Protocol(command, first :: keysAndArgs(keys, args)).using(R)

  final def eval[R, A](script: String)(implicit R: RESPRead.Aux[R, A]): Single[A]                      = run("EVAL", script, Nil, Nil)(R)
  final def eval[R, A](script: String, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVAL", script, keys.value, Nil)(R)
  final def evalargs[R, A](script: String, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVAL", script, Nil, args.value)(R)
  final def eval[R, A](script: String, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVAL", script, keys.value, args.value)(R)

  /** EVAL of a script in the cache, by its SHA1 digest */
  final def evalsha[R, A](sha: Sha1)(implicit R: RESPRead.Aux[R, A]): Single[A] = run("EVALSHA", sha.value, Nil, Nil)(R)
  final def evalsha[R, A](sha: Sha1, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVALSHA", sha.value, keys.value, Nil)(R)
  final def evalshaargs[R, A](sha: Sha1, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVALSHA", sha.value, Nil, args.value)(R)
  final def evalsha[R, A](sha: Sha1, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVALSHA", sha.value, keys.value, args.value)(R)
}

trait ScriptingBaseP extends ScriptingCoreP {
  override private[laserdisc] def supported: List[String] = super.supported ++ script.supported

  lazy val script: ScriptCommands = new ScriptCommands {}
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

  val help: Single[Seq[String]] = Protocol.help("FUNCTION")

  /** Deletes the library and its functions */
  def delete(library: Key): Single[OK] = Protocol("FUNCTION", "DELETE" :: library.value :: Nil).as[Str, OK]

  /** Every library, in a binary form for `restore` */
  val dump: Single[Bulk] = Protocol("FUNCTION", "DUMP").as[Bulk, Bulk]

  /** Deletes every library */
  val flush: Single[OK]                  = Protocol("FUNCTION", "FLUSH").as[Str, OK]
  def flush(mode: FlushMode): Single[OK] = Protocol("FUNCTION", "FLUSH" *: mode *: EmptyTuple).as[Str, OK]

  /** Stops the function running, if it hasn't written anything */
  val kill: Single[OK] = Protocol("FUNCTION", "KILL").as[Str, OK]

  private final val libraries = RESPRead.instance(ScriptingP.libraries)

  /** The libraries, or those whose name matches the pattern */
  val list: Single[Seq[Library]]                       = Protocol("FUNCTION", "LIST").using(libraries)
  def list(pattern: GlobPattern): Single[Seq[Library]] =
    Protocol("FUNCTION", "LIST" *: "LIBRARYNAME" *: pattern *: EmptyTuple).using(libraries)
  def list(withCode: WithCode): Single[Seq[Library]] = Protocol("FUNCTION", "LIST" *: withCode *: EmptyTuple).using(libraries)
  def list(pattern: GlobPattern, withCode: WithCode): Single[Seq[Library]] =
    Protocol("FUNCTION", "LIST" *: "LIBRARYNAME" *: pattern *: withCode *: EmptyTuple).using(libraries)

  /** Loads a library from its code, which starts with `#!lua name=<library>`, and replies with the library's name */
  def load(code: String): Single[Key]                   = Protocol("FUNCTION", "LOAD" :: code :: Nil).as[Bulk, Key]
  def load(code: String, replace: Replace): Single[Key] = Protocol("FUNCTION", "LOAD" *: replace *: code *: EmptyTuple).as[Bulk, Key]

  /** Loads the libraries of a `dump` */
  def restore(payload: Bulk): Single[OK]                        = Protocol("FUNCTION", "RESTORE" *: payload *: EmptyTuple).as[Str, OK]
  def restore(payload: Bulk, policy: RestorePolicy): Single[OK] =
    Protocol("FUNCTION", "RESTORE" *: payload *: policy *: EmptyTuple).as[Str, OK]

  val stats: Single[Stats] = Protocol("FUNCTION", "STATS").using(RESPRead.instance(ScriptingP.stats))
}

/** EVAL_RO and EVALSHA_RO: Redis 7.0 */
trait ScriptingReadOnlyP extends ScriptingCoreP {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("EVAL_RO", "EVALSHA_RO")

  /** EVAL of a script that only reads, which may run on a replica */
  final def evalro[R, A](script: String)(implicit R: RESPRead.Aux[R, A]): Single[A] = run("EVAL_RO", script, Nil, Nil)(R)
  final def evalro[R, A](script: String, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVAL_RO", script, keys.value, Nil)(R)
  final def evalroargs[R, A](script: String, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVAL_RO", script, Nil, args.value)(R)
  final def evalro[R, A](script: String, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVAL_RO", script, keys.value, args.value)(R)

  /** EVALSHA of a script that only reads, which may run on a replica */
  final def evalsharo[R, A](sha: Sha1)(implicit R: RESPRead.Aux[R, A]): Single[A] = run("EVALSHA_RO", sha.value, Nil, Nil)(R)
  final def evalsharo[R, A](sha: Sha1, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVALSHA_RO", sha.value, keys.value, Nil)(R)
  final def evalsharoargs[R, A](sha: Sha1, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVALSHA_RO", sha.value, Nil, args.value)(R)
  final def evalsharo[R, A](sha: Sha1, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("EVALSHA_RO", sha.value, keys.value, args.value)(R)
}

/** EVAL_RO, EVALSHA_RO, FCALL and FCALL_RO, and FUNCTION under `function`. Redis 7.0 */
trait ScriptingFunctionsP extends ScriptingBaseP with ScriptingReadOnlyP {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("FCALL", "FCALL_RO") ++ function.supported

  lazy val function: FunctionCommands = new FunctionCommands {}

  /** Calls a function of a loaded library */
  final def fcall[R, A](function: Key)(implicit R: RESPRead.Aux[R, A]): Single[A] = run("FCALL", function.value, Nil, Nil)(R)
  final def fcall[R, A](function: Key, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("FCALL", function.value, keys.value, Nil)(R)
  final def fcallargs[R, A](function: Key, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("FCALL", function.value, Nil, args.value)(R)
  final def fcall[R, A](function: Key, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("FCALL", function.value, keys.value, args.value)(R)

  /** FCALL of a function flagged `no-writes`, which may run on a replica */
  final def fcallro[R, A](function: Key)(implicit R: RESPRead.Aux[R, A]): Single[A] = run("FCALL_RO", function.value, Nil, Nil)(R)
  final def fcallro[R, A](function: Key, keys: OneOrMoreKeys)(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("FCALL_RO", function.value, keys.value, Nil)(R)
  final def fcallroargs[R, A](function: Key, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("FCALL_RO", function.value, Nil, args.value)(R)
  final def fcallro[R, A](function: Key, keys: OneOrMoreKeys, args: OneOrMore[Value])(implicit R: RESPRead.Aux[R, A]): Single[A] =
    run("FCALL_RO", function.value, keys.value, args.value)(R)
}

trait ScriptAllCommands extends ScriptCommands with ScriptShowCommands

trait ScriptingP extends ScriptingFunctionsP {
  override val scriptingtypes: ScriptingP.ScriptingTypes with ScriptingP.ScriptingFunctionTypes = new ScriptingP.ScriptingTypes
    with ScriptingP.ScriptingFunctionTypes {}

  override lazy val script: ScriptAllCommands = new ScriptAllCommands {}
}
