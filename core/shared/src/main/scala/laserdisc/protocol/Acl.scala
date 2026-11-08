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

/** The replies and rules of ACL */
object Acl {

  /** The rules of ACL SETUSER that every version has, applied in order: whether the user is on, its passwords, the keys and channels it may
    * use, and the commands it may run. Patterns are globs, and commands and categories are names like `get` or `string`
    */
  abstract class Rules[Self] private[Acl] (val params: List[String]) {
    protected def next(params: List[String]): Self

    private def add(rule: String): Self = next(params :+ rule)

    def on: Self                                = add("on")
    def off: Self                               = add("off")
    def nopass: Self                            = add("nopass")
    def resetpass: Self                         = add("resetpass")
    def password(password: Key): Self           = add(s">${password.value}")
    def removePassword(password: Key): Self     = add(s"<${password.value}")
    def hashedPassword(sha256: Key): Self       = add(s"#${sha256.value}")
    def removeHashedPassword(sha256: Key): Self = add(s"!${sha256.value}")
    def keys(pattern: GlobPattern): Self        = add(s"~${pattern.value}")
    def allKeys: Self                           = add("allkeys")
    def resetKeys: Self                         = add("resetkeys")
    def channels(pattern: GlobPattern): Self    = add(s"&${pattern.value}")
    def allChannels: Self                       = add("allchannels")
    def resetChannels: Self                     = add("resetchannels")
    def allow(command: Key): Self               = add(s"+${command.value}")
    def deny(command: Key): Self                = add(s"-${command.value}")
    def allowCategory(category: Key): Self      = add(s"+@${category.value}")
    def denyCategory(category: Key): Self       = add(s"-@${category.value}")
    def allCommands: Self                       = add("allcommands")
    def noCommands: Self                        = add("nocommands")

    /** Removes every rule, leaving the user off, without passwords, keys, channels or commands */
    def reset: Self = add("reset")
  }

  /** The rules of Redis 6.2 */
  final class AclRules private (params: List[String]) extends Rules[AclRules](params) {
    override protected def next(params: List[String]): AclRules = new AclRules(params)
  }
  object AclRules {

    /** No rules, which creates the user as it is: off, without passwords, keys, channels or commands */
    final val none: AclRules = new AclRules(Nil)
  }

  /** Rules that add keys that may only be read or only be written, or both, which is the same as `keys` */
  abstract class KeyPermissionRules[Self] private[Acl] (params: List[String]) extends Rules[Self](params) {
    def readKeys(pattern: GlobPattern): Self      = next(params :+ s"%R~${pattern.value}")
    def writeKeys(pattern: GlobPattern): Self     = next(params :+ s"%W~${pattern.value}")
    def readWriteKeys(pattern: GlobPattern): Self = next(params :+ s"%RW~${pattern.value}")
  }

  /** Rules that add selectors: other sets of rules a command may match instead */
  abstract class SelectorRules[Self <: Rules[Self]] private[Acl] (params: List[String]) extends KeyPermissionRules[Self](params) {
    def selector(rules: Self): Self = next(params :+ rules.params.mkString("(", " ", ")"))
    def clearSelectors: Self        = next(params :+ "clearselectors")
  }

  /** The rules of Redis 7.0, with keys that may only be read or only be written, and selectors */
  final class AclRulesWithSelectors private (params: List[String]) extends SelectorRules[AclRulesWithSelectors](params) {
    override protected def next(params: List[String]): AclRulesWithSelectors = new AclRulesWithSelectors(params)
  }
  private[laserdisc] object AclRulesWithSelectors {
    final val none: AclRulesWithSelectors = new AclRulesWithSelectors(Nil)
  }

  /** The rules of Dragonfly, which add keys that may only be read or only be written, but not selectors */
  final class AclDragonflyRules private (params: List[String]) extends KeyPermissionRules[AclDragonflyRules](params) {
    override protected def next(params: List[String]): AclDragonflyRules = new AclDragonflyRules(params)
  }
  private[laserdisc] object AclDragonflyRules {
    final val none: AclDragonflyRules = new AclDragonflyRules(Nil)
  }

  /** The rules of Valkey 9.0, which add the databases a user, or a selector, may use
    */
  final class AclRulesWithDatabases private (params: List[String]) extends SelectorRules[AclRulesWithDatabases](params) {
    override protected def next(params: List[String]): AclRulesWithDatabases = new AclRulesWithDatabases(params)

    def allDatabases: AclRulesWithDatabases = next(params :+ "alldbs")

    /** Only these databases, instead of the ones allowed before */
    def databases(indexes: OneOrMore[DbIndex]): AclRulesWithDatabases =
      next(params :+ indexes.value.map(Show[DbIndex].show).mkString("db=", ",", ""))
    def resetDatabases: AclRulesWithDatabases = next(params :+ "resetdbs")
  }
  private[laserdisc] object AclRulesWithDatabases {
    final val none: AclRulesWithDatabases = new AclRulesWithDatabases(Nil)
  }

  /** A set of rules of a user's selectors, from Redis 7.0 */
  sealed trait AclSelector7_0 {
    def commands: String
    def keys: String
    def channels: String
  }

  /** A set of rules of a user's selectors from Valkey 9.1, with the databases it may use, `alldbs` when not limited */
  sealed trait AclSelectorValkey9_1 extends AclSelector7_0 {
    def databases: String
  }

  /** ACL GETUSER: the user's flags (like `on`), the SHA-256 of its passwords, and its command, key and channel rules, as rule text like
    * `~app:*`
    */
  sealed trait AclUser6_2 {
    def flags: Seq[String]
    def passwords: Seq[String]
    def commands: String
    def keys: String
    def channels: String
  }

  /** ACL GETUSER from Redis 7.0, with the user's selectors */
  sealed trait AclUser7_0 extends AclUser6_2 {
    def selectors: Seq[AclSelector7_0]
  }

  /** ACL GETUSER from Valkey 9.1, with the databases the user, and each selector, may use, `alldbs` when not limited */
  sealed trait AclUserValkey9_1 extends AclUser7_0 {
    override def selectors: Seq[AclSelectorValkey9_1]
    def databases: String
  }

  private[laserdisc] final case class AclSelectorOf7_0(commands: String, keys: String, channels: String) extends AclSelector7_0
  private[laserdisc] final case class AclSelectorOfValkey9_1(commands: String, keys: String, channels: String, databases: String)
      extends AclSelectorValkey9_1
  private[laserdisc] final case class AclUserOf6_2(
      flags: Seq[String],
      passwords: Seq[String],
      commands: String,
      keys: String,
      channels: String
  ) extends AclUser6_2
  private[laserdisc] final case class AclUserOf7_0(
      flags: Seq[String],
      passwords: Seq[String],
      commands: String,
      keys: String,
      channels: String,
      selectors: Seq[AclSelector7_0]
  ) extends AclUser7_0
  private[laserdisc] final case class AclUserOfValkey9_1(
      flags: Seq[String],
      passwords: Seq[String],
      commands: String,
      keys: String,
      channels: String,
      selectors: Seq[AclSelectorValkey9_1],
      databases: String
  ) extends AclUserValkey9_1

  // Dragonfly replies in simple strings, with a string of rules or an empty array for none
  private[protocol] final val maybeUserDragonfly: RESPRead[Option[AclUser6_2]] = {
    def text(resp: RESP): String = resp match {
      case Str(s)  => s
      case Bulk(s) => s
      case _       => ""
    }
    implicit val user: Read[Arr, AclUser6_2] = Read.instance { reply =>
      val fields             = reply.elements.grouped(2).collect { case Seq(name, value) => text(name) -> value }.toMap
      def many(name: String) = fields.get(name) match {
        case Some(Arr(elements)) => elements.map(text)
        case Some(other)         => text(other).split(' ').toList.filter(_.nonEmpty)
        case None                => Nil
      }
      def rules(name: String) = many(name).mkString(" ")
      Right(AclUserOf6_2(many("flags"), many("passwords"), rules("commands"), rules("keys"), rules("channels")))
    }
    RESPRead[OrNullBulk[Arr], Option[AclUser6_2]]
  }

  // what keys and channels hold: Redis 6.2 lists bare patterns, Redis 7.0 replies with the rules themselves
  private final case class Patterns(withPrefix: String => String)
  private implicit final val bulkPatterns: Read[Bulk, Patterns]         = Read.infallible(bulk => Patterns(_ => bulk.value))
  private implicit final val barePatterns: Read[Arr.Of[Bulk], Patterns] = Read[Arr.Of[Bulk], Seq[String]].map { patterns =>
    Patterns(prefix => patterns.map(p => s"$prefix$p").mkString(" "))
  }

  private final def selector(f: Fields): Either[RESPDecErr, AclSelectorOf7_0] =
    for {
      commands <- f.bulk[String]("commands")
      keys     <- f.bulk[String]("keys")
      channels <- f.bulk[String]("channels")
    } yield AclSelectorOf7_0(commands, keys, channels)
  private implicit final val selector7_0: Read[Record, AclSelector7_0]             = Read.record("ACL selector")(selector)
  private implicit final val selectorValkey9_1: Read[Record, AclSelectorValkey9_1] = Read.record("ACL selector") { f =>
    for {
      sel       <- selector(f)
      databases <- f.bulk[String]("databases")
    } yield AclSelectorOfValkey9_1(sel.commands, sel.keys, sel.channels, databases)
  }
  // no such user is a null bulk string
  private final def maybeUser[U](read: Read[Record, U]): RESPRead[Option[U]] = {
    implicit val user: Read[Record, U] = read
    RESPRead[OrNullBulk[Record], Option[U]]
  }

  // what Redis 6.2 replies, which also takes what later versions reply, so that a server can be upgraded before the import moves on
  private[protocol] final val maybeUser6_2: RESPRead[Option[AclUser6_2]] = maybeUser(Read.record("ACL user") { f =>
    for {
      flags     <- f[Arr.Of[Bulk], Seq[String]]("flags")
      passwords <- f[Arr.Of[Bulk], Seq[String]]("passwords")
      commands  <- f.bulk[String]("commands")
      keys      <- f[OneOf[Bulk *: Arr.Of[Bulk] *: EmptyTuple], Patterns]("keys")
      channels  <- f[OneOf[Bulk *: Arr.Of[Bulk] *: EmptyTuple], Patterns]("channels")
    } yield AclUserOf6_2(flags, passwords, commands, keys.withPrefix("~"), channels.withPrefix("&"))
  })

  // Redis 7.0 replies with the rules, and with the selectors
  private final def user7_0(f: Fields): Either[RESPDecErr, AclUserOf6_2] =
    for {
      flags     <- f[Arr.Of[Bulk], Seq[String]]("flags")
      passwords <- f[Arr.Of[Bulk], Seq[String]]("passwords")
      commands  <- f.bulk[String]("commands")
      keys      <- f.bulk[String]("keys")
      channels  <- f.bulk[String]("channels")
    } yield AclUserOf6_2(flags, passwords, commands, keys, channels)
  private[protocol] final val maybeUser7_0: RESPRead[Option[AclUser7_0]] = maybeUser(Read.record("ACL user") { f =>
    for {
      u         <- user7_0(f)
      selectors <- f[Records, Seq[AclSelector7_0]]("selectors")
    } yield AclUserOf7_0(u.flags, u.passwords, u.commands, u.keys, u.channels, selectors)
  })

  // Valkey 9.1 adds the databases, to the user and to each selector
  private[protocol] final val maybeUserValkey9_1: RESPRead[Option[AclUserValkey9_1]] = maybeUser(Read.record("ACL user") { f =>
    for {
      u         <- user7_0(f)
      selectors <- f[Records, Seq[AclSelectorValkey9_1]]("selectors")
      databases <- f.bulk[String]("databases")
    } yield AclUserOfValkey9_1(u.flags, u.passwords, u.commands, u.keys, u.channels, selectors, databases)
  })

  /** An entry of ACL LOG: what was denied (`reason` like `auth` or `command`, `object` the command, key or channel), to whom, how many
    * times and how long ago
    */
  sealed trait AclLogEntry6_2 {
    def count: Long
    def reason: String
    def context: String
    def denied: String
    def username: String
    def ageSeconds: Double
    def clientInfo: String
  }

  /** An entry of ACL LOG from Redis 7.2: also its id, and when it was created and last updated, in milliseconds */
  sealed trait AclLogEntry7_2 extends AclLogEntry6_2 {
    def entryId: Long
    def createdAt: Long
    def updatedAt: Long
  }

  private[laserdisc] final case class AclLogEntryOf6_2(
      count: Long,
      reason: String,
      context: String,
      denied: String,
      username: String,
      ageSeconds: Double,
      clientInfo: String
  ) extends AclLogEntry6_2
  private[laserdisc] final case class AclLogEntryOf7_2(
      count: Long,
      reason: String,
      context: String,
      denied: String,
      username: String,
      ageSeconds: Double,
      clientInfo: String,
      entryId: Long,
      createdAt: Long,
      updatedAt: Long
  ) extends AclLogEntry7_2

  private final def logEntry(f: Fields): Either[RESPDecErr, AclLogEntryOf6_2] =
    for {
      count    <- f.num[Long]("count")
      reason   <- f.bulk[String]("reason")
      context  <- f.bulk[String]("context")
      denied   <- f.bulk[String]("object")
      username <- f.bulk[String]("username")
      age      <- f.bulk[Double]("age-seconds")
      info     <- f.bulk[String]("client-info")
    } yield AclLogEntryOf6_2(count, reason, context, denied, username, age, info)
  private implicit final val logEntry6_2: Read[Record, AclLogEntry6_2] = Read.record("ACL log entry")(logEntry)
  private implicit final val logEntry7_2: Read[Record, AclLogEntry7_2] = Read.record("ACL log entry") { f =>
    for {
      e         <- logEntry(f)
      entryId   <- f.num[Long]("entry-id")
      createdAt <- f.num[Long]("timestamp-created")
      updatedAt <- f.num[Long]("timestamp-last-updated")
    } yield AclLogEntryOf7_2(e.count, e.reason, e.context, e.denied, e.username, e.ageSeconds, e.clientInfo, entryId, createdAt, updatedAt)
  }
  private[protocol] final val logEntries6_2: Read[Records, Seq[AclLogEntry6_2]] = Read[Records, Seq[AclLogEntry6_2]]
  private[protocol] final val logEntries7_2: Read[Records, Seq[AclLogEntry7_2]] = Read[Records, Seq[AclLogEntry7_2]]
}

/** ACL: Redis 6.2 */
trait AclCommands extends Features {
  import Acl.{AclLogEntry6_2, AclRules, AclUser6_2}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "ACL CAT",
      "ACL DELUSER",
      "ACL GENPASS",
      "ACL GETUSER",
      "ACL HELP",
      "ACL LIST",
      "ACL LOAD",
      "ACL LOG: RESET",
      "ACL SAVE",
      "ACL SETUSER",
      "ACL USERS",
      "ACL WHOAMI"
    )

  val help: Single[Seq[String]] = Protocol.help("ACL")

  /** The ACL categories, or the commands of one */
  val cat: Single[Seq[String]]                = Protocol("ACL", "CAT").as[Texts, Seq[String]]
  def cat(category: Key): Single[Seq[String]] = Protocol("ACL", "CAT" :: category.value :: Nil).as[Texts, Seq[String]]

  /** Deletes the users, and replies how many it deleted */
  def deluser(users: OneOrMoreKeys): Single[NonNegLong] = Protocol("ACL", "DELUSER" :: users.value.map(_.value)).as[Num, NonNegLong]

  /** A random password, as hex: 256 bits, or `bits` */
  val genpass: Single[String]               = Protocol("ACL", "GENPASS").as[OneOf[Bulk *: Str *: EmptyTuple], String]
  def genpass(bits: PosInt): Single[String] =
    Protocol("ACL", "GENPASS" *: bits *: EmptyTuple).as[OneOf[Bulk *: Str *: EmptyTuple], String]

  /** The user's rules, or `None` when there's no such user */
  def getuser(user: Key): Single[Option[AclUser6_2]] = Protocol("ACL", "GETUSER" :: user.value :: Nil).using(Acl.maybeUser6_2)

  /** Every user's rules, as the lines of an ACL file */
  val list: Single[Seq[String]] = Protocol("ACL", "LIST").as[Texts, Seq[String]]

  val load: Single[OK] = Protocol("ACL", "LOAD").as[Str, OK]

  /** The 10 most recent denials, or `count` of them */
  val log: Single[Seq[AclLogEntry6_2]]                = Protocol("ACL", "LOG").using(RESPRead.instance(Acl.logEntries6_2))
  def log(count: PosInt): Single[Seq[AclLogEntry6_2]] =
    Protocol("ACL", "LOG" *: count *: EmptyTuple).using(RESPRead.instance(Acl.logEntries6_2))
  val logreset: Single[OK] = Protocol("ACL", "LOG" :: "RESET" :: Nil).as[Str, OK]

  val save: Single[OK] = Protocol("ACL", "SAVE").as[Str, OK]

  /** Creates the user, or changes it, applying the rules in order */
  def setuser(user: Key, rules: AclRules): Single[OK] = Protocol("ACL", "SETUSER" :: user.value :: rules.params).as[Str, OK]

  val users: Single[Seq[String]] = Protocol("ACL", "USERS").as[Texts, Seq[String]]

  val whoami: Single[String] = Protocol("ACL", "WHOAMI").as[Bulk, String]
}

/** ACL DRYRUN: Redis 7.0 */
trait AclDryRunCommands extends Features {
  override private[laserdisc] def supported: List[String] = super.supported ++ List("ACL DRYRUN")

  /** Whether the user may run the command, without running it: `None` when it may, or the reason when it may not */
  def dryrun(user: Key, command: Single[Any]): Single[Option[String]] =
    Protocol("ACL", "DRYRUN" :: user.value :: ServerInspection.argumentsOf(command)).using(RESPRead.okOrMessage)
}

/** ACL as Dragonfly has it: without selectors, and it refuses the rules `reset`, `hashedPassword` and `removeHashedPassword` */
trait AclDragonflyCommands extends AclCommands with AclDryRunCommands {
  override def getuser(user: Key): Single[Option[Acl.AclUser6_2]] =
    Protocol("ACL", "GETUSER" :: user.value :: Nil).using(Acl.maybeUserDragonfly)

  def setuser(user: Key, rules: Acl.AclDragonflyRules): Single[OK] =
    Protocol("ACL", "SETUSER" :: user.value :: rules.params).as[Str, OK]
}

/** ACL DRYRUN and ACL SETUSER with selectors: Redis 7.0 */
trait AclSelectorCommands extends AclCommands with AclDryRunCommands {
  import Acl.{AclRulesWithSelectors, AclUser7_0}

  override def getuser(user: Key): Single[Option[AclUser7_0]] = Protocol("ACL", "GETUSER" :: user.value :: Nil).using(Acl.maybeUser7_0)

  def setuser(user: Key, rules: AclRulesWithSelectors): Single[OK] =
    Protocol("ACL", "SETUSER" :: user.value :: rules.params).as[Str, OK]
}

/** ACL LOG with each entry's id and when it was created and last updated: Redis 7.2 */
trait AclLog7_2Commands extends AclCommands {
  import Acl.AclLogEntry7_2

  override val log: Single[Seq[AclLogEntry7_2]]                = Protocol("ACL", "LOG").using(RESPRead.instance(Acl.logEntries7_2))
  override def log(count: PosInt): Single[Seq[AclLogEntry7_2]] =
    Protocol("ACL", "LOG" *: count *: EmptyTuple).using(RESPRead.instance(Acl.logEntries7_2))
}

/** ACL SETUSER with the databases a user may use, and ACL GETUSER with them: Valkey 9.1 */
trait AclDatabaseCommands extends AclSelectorCommands {
  override def getuser(user: Key): Single[Option[Acl.AclUserValkey9_1]] =
    Protocol("ACL", "GETUSER" :: user.value :: Nil).using(Acl.maybeUserValkey9_1)

  def setuser(user: Key, rules: Acl.AclRulesWithDatabases): Single[OK] =
    Protocol("ACL", "SETUSER" :: user.value :: rules.params).as[Str, OK]
}
