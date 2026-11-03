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

  /** The rules of Redis 7.0, which add keys that may only be read or only be written, and selectors: other sets of rules a command may
    * match instead
    */
  final class AclRulesWithSelectors private (params: List[String]) extends Rules[AclRulesWithSelectors](params) {
    override protected def next(params: List[String]): AclRulesWithSelectors = new AclRulesWithSelectors(params)

    def readKeys(pattern: GlobPattern): AclRulesWithSelectors         = next(params :+ s"%R~${pattern.value}")
    def writeKeys(pattern: GlobPattern): AclRulesWithSelectors        = next(params :+ s"%W~${pattern.value}")
    def selector(rules: AclRulesWithSelectors): AclRulesWithSelectors = next(params :+ rules.params.mkString("(", " ", ")"))
    def clearSelectors: AclRulesWithSelectors                         = next(params :+ "clearselectors")
  }
  object AclRulesWithSelectors {
    final val none: AclRulesWithSelectors = new AclRulesWithSelectors(Nil)
  }

  /** A set of rules of a user's selectors */
  final case class AclSelector(commands: String, keys: String, channels: String)

  /** ACL GETUSER: the user's flags (like `on`), the SHA-256 of its passwords, and its command, key and channel rules, as rule text like
    * `~app:*`. Redis 7.0 adds selectors, and Valkey 9.0 the databases the user may use
    */
  final case class AclUser(
      flags: Seq[String],
      passwords: Seq[String],
      commands: String,
      keys: String,
      channels: String,
      selectors: Seq[AclSelector],
      databases: Option[String]
  )

  private final def texts(resp: RESP): Option[Seq[String]] = resp match {
    case Arr(es) =>
      val read = es.map { case Bulk(s) => Some(s); case Str(s) => Some(s); case _ => None }
      if (read.forall(_.isDefined)) Some(read.flatten) else None
    case _ => None
  }

  // Redis 6.2 lists bare patterns; later versions reply with the rules themselves
  private final def rules(resp: RESP, prefix: String): Option[String] = resp match {
    case Bulk(s)  => Some(s)
    case arr: Arr => texts(arr).map(_.map(p => s"$prefix$p").mkString(" "))
    case _        => None
  }

  private final val selector: Read[Arr, AclSelector] = Read.record("ACL selector") { m =>
    def text(k: String) = m.get(k).collect { case Bulk(s) => s }
    for {
      c  <- text("commands")
      k  <- text("keys")
      ch <- text("channels")
    } yield AclSelector(c, k, ch)
  }
  private final val selectors: Read[Arr, Seq[AclSelector]] = Read.arrOf(selector)

  private[protocol] final val user: Read[Arr, AclUser] = Read.record("ACL user") { m =>
    for {
      flags     <- m.get("flags").flatMap(texts)
      passwords <- m.get("passwords").flatMap(texts)
      commands  <- m.get("commands").collect { case Bulk(c) => c }
      keys      <- m.get("keys").flatMap(rules(_, "~"))
      channels  <- m.get("channels").fold[Option[String]](Some(""))(rules(_, "&"))
      selected  <- m.get("selectors").fold[Option[Seq[AclSelector]]](Some(Nil)) { case selectors(Right(ss)) => Some(ss); case _ => None }
    } yield AclUser(flags, passwords, commands, keys, channels, selected, m.get("databases").collect { case Bulk(d) => d })
  }

  /** An entry of ACL LOG: what was denied (`reason` like `auth` or `command`, `object` the command, key or channel), to whom, how many
    * times and how long ago. Redis 7.2 adds the entry's id and when it was created and last updated, in milliseconds
    */
  final case class AclLogEntry(
      count: Long,
      reason: String,
      context: String,
      denied: String,
      username: String,
      ageSeconds: Double,
      clientInfo: String,
      entryId: Option[Long],
      createdAt: Option[Long],
      updatedAt: Option[Long]
  )

  private final val logEntry: Read[Arr, AclLogEntry] = Read.record("ACL log entry") { m =>
    def text(k: String)   = m.get(k).collect { case Bulk(s) => s }
    def number(k: String) = m.get(k).collect { case Num(n) => n }
    for {
      count    <- number("count")
      reason   <- text("reason")
      context  <- text("context")
      denied   <- text("object")
      username <- text("username")
      age      <- text("age-seconds").collect { case ToDouble(d) => d }
      info     <- text("client-info")
    } yield AclLogEntry(
      count,
      reason,
      context,
      denied,
      username,
      age,
      info,
      number("entry-id"),
      number("timestamp-created"),
      number("timestamp-last-updated")
    )
  }
  private[protocol] final val logEntries: Read[Arr, Seq[AclLogEntry]] = Read.arrOf(logEntry)
}

/** ACL: Redis 6.2 */
trait AclCommands extends Features {
  import Acl.{AclLogEntry, AclRules, AclUser}

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "ACL CAT",
      "ACL DELUSER",
      "ACL GENPASS",
      "ACL GETUSER",
      "ACL LIST",
      "ACL LOAD",
      "ACL LOG: RESET",
      "ACL SAVE",
      "ACL SETUSER",
      "ACL USERS",
      "ACL WHOAMI"
    )

  /** The ACL categories, or the commands of one */
  val cat: Single.Aux[Seq[String]]                = Protocol("ACL", "CAT").as[Arr.Of[Bulk], Seq[String]]
  def cat(category: Key): Single.Aux[Seq[String]] = Protocol("ACL", "CAT" :: category.value :: Nil).as[Arr.Of[Bulk], Seq[String]]

  /** Deletes the users, and replies how many it deleted */
  def deluser(users: OneOrMoreKeys): Single.Aux[NonNegLong] = Protocol("ACL", "DELUSER" :: users.value.map(_.value)).as[Num, NonNegLong]

  /** A random password, as hex: 256 bits, or `bits` */
  val genpass: Single.Aux[String]               = Protocol("ACL", "GENPASS").as[Bulk, String]
  def genpass(bits: PosInt): Single.Aux[String] = Protocol("ACL", "GENPASS" *: bits *: EmptyTuple).as[Bulk, String]

  /** The user's rules, or `None` when there's no such user */
  def getuser(user: Key): Single.Aux[Option[AclUser]] =
    Protocol("ACL", "GETUSER" :: user.value :: Nil).using(RESPRead.nullableArr(Acl.user))

  /** Every user's rules, as the lines of an ACL file */
  val list: Single.Aux[Seq[String]] = Protocol("ACL", "LIST").as[Arr.Of[Bulk], Seq[String]]

  val load: Single.Aux[OK] = Protocol("ACL", "LOAD").as[Str, OK]

  /** The 10 most recent denials, or `count` of them */
  val log: Single.Aux[Seq[AclLogEntry]]                = Protocol("ACL", "LOG").using(RESPRead.instance(Acl.logEntries))
  def log(count: PosInt): Single.Aux[Seq[AclLogEntry]] =
    Protocol("ACL", "LOG" *: count *: EmptyTuple).using(RESPRead.instance(Acl.logEntries))
  val logreset: Single.Aux[OK] = Protocol("ACL", "LOG" :: "RESET" :: Nil).as[Str, OK]

  val save: Single.Aux[OK] = Protocol("ACL", "SAVE").as[Str, OK]

  /** Creates the user, or changes it, applying the rules in order */
  def setuser(user: Key, rules: AclRules): Single.Aux[OK] = Protocol("ACL", "SETUSER" :: user.value :: rules.params).as[Str, OK]

  val users: Single.Aux[Seq[String]] = Protocol("ACL", "USERS").as[Arr.Of[Bulk], Seq[String]]

  val whoami: Single.Aux[String] = Protocol("ACL", "WHOAMI").as[Bulk, String]
}

/** ACL DRYRUN and ACL SETUSER with selectors: Redis 7.0 */
trait AclSelectorCommands extends Features {
  import Acl.AclRulesWithSelectors

  override private[laserdisc] def supported: List[String] = super.supported ++ List("ACL DRYRUN")

  /** Whether the user may run the command, without running it: `None` when it may, or the reason when it may not */
  def dryrun(user: Key, command: Single): Single.Aux[Option[String]] =
    Protocol("ACL", "DRYRUN" :: user.value :: ServerInspection.argumentsOf(command)).using(RESPRead.okOrMessage)

  def setuser(user: Key, rules: AclRulesWithSelectors): Single.Aux[OK] =
    Protocol("ACL", "SETUSER" :: user.value :: rules.params).as[Str, OK]
}
