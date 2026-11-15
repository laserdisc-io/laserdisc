# Commands

A command is a value of type `Protocol[A]`, where `A` is the type of its
reply. Building one doesn't send anything; you send it with a
[client](client.md).

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*

val setA: Protocol[OK]             = set("a", 23)
val getA: Protocol[Option[PosInt]] = get[PosInt]("a")
```

`laserdisc.redis_8_10.*` brings every command of Redis 8.10 into scope. Use the
import for your server: see
[The version of your server](#the-version-of-your-server).

To import a single family instead, add its name:

| Family | Import |
| --- | --- |
| Strings | `laserdisc.redis_8_10.strings.*` |
| Streams | `laserdisc.redis_8_10.streams.*` |
| Keys | `laserdisc.redis_8_10.keys.*` |
| Hashes | `laserdisc.redis_8_10.hashmaps.*` |
| Arrays, from Redis 8.8 | `laserdisc.redis_8_10.arrays.*` |
| Lists | `laserdisc.redis_8_10.lists.*` |
| Sets | `laserdisc.redis_8_10.sets.*` |
| Sorted sets | `laserdisc.redis_8_10.sortedsets.*` |
| Geospatial | `laserdisc.redis_8_10.geo.*` |
| HyperLogLog | `laserdisc.redis_8_10.hyperloglog.*` |
| Publish | `laserdisc.redis_8_10.publish.*` |
| Transactions | `laserdisc.redis_8_10.transaction.*` |
| Scripting | `laserdisc.redis_8_10.scripting.*` |
| Connection | `laserdisc.redis_8_10.connection.*` |
| Server | `laserdisc.redis_8_10.server.*` |
| Cluster | `laserdisc.redis_8_10.cluster.*` |
| Blocking commands of lists, sorted sets and streams | `laserdisc.redis_8_10.blocking.*` |
| Blocking commands with no time limit: see [Client](client.md#waiting-for-as-long-as-it-takes) | `laserdisc.redis_8_10.blocking.forever.*` |
| Dragonfly's own commands: see [Dragonfly](#dragonfly) | `laserdisc.dragonfly_2_0.dragonfly.*` |

Commands whose name starts with a group, such as `CLIENT LIST` or
`ACL SETUSER`, are under that group's name: `client.list`,
`acl.setuser(user, rules)`. In `redis_8_10`, the groups are `acl`, `backup`,
`client`, `command`, `config`, `function`, `hotkeys`, `latency`, `memory`,
`migration`, `module`, `obj`, `pubsub`, `ro`, `script` and `slowlog`. `obj` is
for `OBJECT`, as `object` is a Scala keyword, `migration` for
`CLUSTER MIGRATION`, and `ro` for the read-only `GEORADIUS_RO` and
`GEORADIUSBYMEMBER_RO`. The Valkey imports from 8.1 also have `commandlog`. An
import for an older version has only the groups whose commands that version
has.

The other commands of `CLUSTER`, `XGROUP` and `XINFO` aren't in a group: each
is one word, such as `clusterinfo`, `keyslot`, `xgroupcreate` and
`xinfostream`.

## Arguments

Arguments use types that only accept valid values. For example, a `Key` is a
non-empty string, a `PosInt` is an `Int` greater than zero, and a `Port` is an
`Int` between 1 and 65535.

With `laserdisc.auto.*` imported, you can pass literal values directly. They're
checked when your code compiles, so this doesn't compile:

```scala
set("", 23) // doesn't compile: a key can't be empty
```

Values that are only known at runtime go through `from`, which returns an
`Either` with an error message on the left:

```scala mdoc:compile-only
import laserdisc.*

def keyOf(name: String): Either[String, Key] = Key.from(name)
```

`unsafeFrom` returns the value directly and throws an
`IllegalArgumentException` if it isn't valid. To get the plain value back, use
`.value`.

A command's options are in the `types` object of its family, such as
`stringtypes` for strings or `servertypes` for the server's commands:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*

// SET a 2 IFEQ 1: set a to 2 only if its value is 1
val setIfOne = set("a", 2, stringtypes.ifEq(1))
```

## Replies

Commands that return a value let you choose its type. `get[PosInt]("a")` reads
the value at `a` as a `PosInt`; `get[String]("a")` reads it as a `String`.

If the reply can't be read as that type, the result is an error rather than a
wrong value. If the key doesn't exist, `get` gives `None`.

The server's replies come in a few kinds, which LaserDisc names after the Redis
protocol, RESP: `Str` is a short status such as `OK`, `Bulk` a string, `Num` an
integer, `Arr` an array, `Err` an error, and `NullBulk` and `NilArr` stand for
nil. LaserDisc reads replies in RESP2, version 2 of the protocol. The kinds
matter when you read a type of your own, and for scripts.

## Your own types

To store values of your own types, give LaserDisc a `Show` to write them and a
`Read` to read them back:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*

final case class UserId(value: Long)

// written as the Long it holds, and read back from one
implicit val userIdShow: Show[UserId]       = Show[Long].contramap(_.value)
implicit val userIdRead: Read[Bulk, UserId] = Read[Bulk, Long].map(UserId(_))

val save = set("user", UserId(42L))
val load = get[UserId]("user")
```

`contramap` writes a value as one of another type is written, and `map` reads it
from one. For a value that isn't like any other, `Show.instance` turns it into
the text Redis stores. `Read.instance` lets you read a reply yourself, and fail
with a `RESPDecErr` when a value doesn't fit:

```scala mdoc:compile-only
import laserdisc.*

final case class Score(points: Long)

// a score can't be negative: a stored one that is fails to read, saying why
implicit val scoreRead: Read[Bulk, Score] = Read.instance { bulk =>
  Read[Bulk, Long].read(bulk).flatMap { n =>
    if (n >= 0) Right(Score(n)) else Left(RESPDecErr(s"a score can't be negative, but was $n"))
  }
}
```

## Hashes

You can store a case class in a hash, one field per field of the class, and
read it back:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*

final case class User(name: String, age: Int, email: Option[String])

val save = hset("user:1", User("ada", 36, None))
val load = hgetall[User]("user:1")
```

Fields are found by name, so their order in the hash doesn't matter, and fields
the class doesn't have are ignored. An `Option` field that's `None` isn't
written, and one that's missing from the hash is read as `None`. Any other
missing field is an error that names it.

Each field is written as one value, so its type needs a `Show`. A field that is
itself a case class or a tuple needs a `Show` written for it.

`hgetall` also reads a hash as a `Map`, like `hgetall[Map[Key, String]]`, or as
a list of pairs, like `hgetall[Seq[(Key, Int)]]`.

`hmget` reads the fields you name, in the same order, each as its own type.
Use an `Option` for a field that may be missing; any other missing field is an
error that says which one:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*

val profile: Protocol[(Int, Option[String])] = hmget[Int, Option[String]]("user:1", "age", "email")
```

When the fields are in a list, pass them as `OneOrMoreKeys` and give the types
as a tuple:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*

val fields  = OneOrMoreKeys.unsafeFrom(List(Key("age"), Key("email")))
val profile = hmget[Int *: Option[String] *: EmptyTuple](Key("user:1"), fields)
```

`mget` works the same way for keys. When you don't know how many keys you'll
have, it can also read them into a `Seq`, like `mget[Seq[Option[Int]]](keys)`.

## Scripts

A script can return anything, so `eval`, `evalsha` and `fcall` take two types:
the kind of reply the script sends, and the type to read it as. The keys and the
arguments are the script's `KEYS` and `ARGV`. The arguments are `Value`s: a
value of any type that has a `Show`, such as a number or a string, becomes
one, as `1` and `"two"` do here:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*

val keys = OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
val args = OneOrMore.unsafeFrom(List[Value](1, "two"))

// a number
val count: Protocol[Long] = eval[Num, Long]("return #KEYS + #ARGV", keys, args)
// a table of numbers
val counts: Protocol[Seq[Long]] = eval[Arr.Of[Num], Seq[Long]]("return {#KEYS, #ARGV}", keys, args)
// a string, or nil
val value: Protocol[Option[String]] = eval[OrNullBulk[Bulk], Option[String]]("return redis.call('GET', KEYS[1])", keys)
// always nil
val done: Protocol[Unit] = evalargs[NullBulk, Unit]("redis.log(redis.LOG_NOTICE, ARGV[1])", args)
```

The server turns what the script returns into one of the kinds of reply in
[Replies](#replies):

| The script returns | The reply is |
| --- | --- |
| A number | `Num`, cut to a whole number: return a string for a decimal |
| A string | `Bulk` |
| A table | `Arr`, with the table's elements up to the first nil |
| `true` | `Num`, 1 |
| `false` or `nil` | `NullBulk`, which stands for nil |
| `redis.status_reply(...)` | `Str` |
| `redis.error_reply(...)` | `Err`, which gives a `Left` |

Dragonfly differs in two ways: a number that isn't whole comes as a `Bulk`
holding it, such as `"3.7"`, and a table keeps its nils, as `NullBulk`s, up to
its last element.

When the script may return nil instead of a value, say so in the reply's kind:
`OrNullBulk[Bulk]` for a string, `OrNullBulk[Num]` for a number, and
`OrNilArr[Arr.Of[Num]]` for a table. Then read it as an `Option`, which is
`None` for nil.

Without keys, use `evalargs`, `evalshaargs` or `fcallargs`.

## Transactions

`multi` runs several commands as a transaction: Redis runs them one after the
other, with no command from anyone else in between. You send it like any other
command:

```scala mdoc:compile-only
import cats.effect.IO

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

def setAndIncrement(client: RedisClient[IO]): IO[Maybe[Option[(Maybe[OK], Maybe[Long])]]] =
  client.send(multi(set("a", 1), incr[Long]("a")))
```

The result is:

- `Right(Some(...))` with one result per command. Redis runs every command even
  if one of them fails, and doesn't undo the ones before it, so each result is a
  `Maybe` of its own: a `Right`, or a `Left` with the error, as for a `send`.
- `Right(None)` when nothing ran because a watched key changed (see below).
- `Left` when Redis refuses one of the commands, for example one it doesn't
  know. Nothing runs, and the error says why.

To make a transaction depend on values you read first, send `watch` with
their keys before reading them. If anyone changes one of those keys before
the transaction runs, nothing runs and you get `None`, so you can read the
values again and retry.

A watch belongs to the connection, and a client sends everything over one
connection. Other commands sent through the same client between your `watch`
and your `multi` can cancel the watch, so give code that uses `watch` a client
of its own.

If you decide not to send the transaction after all, send `unwatch`.
Otherwise the watch stays, and the next transaction sent through that client
can get `None`, or fail with `WatchLost`, because of keys it never watched.

If the connection closes between your `watch` and your `multi`, the server
drops the watch with it, even if you watch more keys after that. The client
then cancels the transaction before any of its commands are sent, and `multi`
fails with `WatchLost`: send `watch` again, read the values again and retry.

## The version of your server

Each server and version has its own import, with only what that server has:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.valkey_8_1.*
import laserdisc.auto.*

val setIfOne = set("a", 2, stringtypes.ifEq(1))
```

A command or option your server doesn't have won't compile. For example,
`stringtypes.ifEq` arrived in Valkey 8.1 and Redis 8.4, so the same line doesn't
compile with `laserdisc.redis_8_2.*`.

| Server | Import |
| --- | --- |
| Redis 6.2 to 8.10 | `laserdisc.redis_6_2.*`, `laserdisc.redis_7_0.*`, `laserdisc.redis_7_2.*`, `laserdisc.redis_7_4.*`, `laserdisc.redis_8_0.*`, `laserdisc.redis_8_2.*`, `laserdisc.redis_8_4.*`, `laserdisc.redis_8_6.*`, `laserdisc.redis_8_8.*`, `laserdisc.redis_8_10.*` |
| Valkey 7.2 to 9.1 | `laserdisc.valkey_7_2.*`, `laserdisc.valkey_8_0.*`, `laserdisc.valkey_8_1.*`, `laserdisc.valkey_9_0.*`, `laserdisc.valkey_9_1.*` |
| Dragonfly 2.0 | `laserdisc.dragonfly_2_0.*` |
| KeyDB | `laserdisc.redis_6_2.*` |

Each import matches the latest patch release of its version, so a command added
in a patch release needs that release or a later one. For example, `HOTKEYS HELP`
in `laserdisc.redis_8_6.*` needs Redis 8.6.1 or later.

KeyDB's latest release is based on Redis 6.2, so use `laserdisc.redis_6_2.*`. It
doesn't have `LOLWUT`, and LaserDisc doesn't have KeyDB's own commands.

An import also works with later versions of its server, so use the import for
the oldest version you run. When you upgrade, upgrade your servers first and
change the import afterwards. A later import can send commands an older server
doesn't have, and can't read some of its replies. For example,
`laserdisc.redis_7_0.*` can't read `XAUTOCLAIM` from Redis 6.2.

One command was removed in a later version: Redis 7.0 replaced `STRALGO` with
`LCS`. So `stralgolcs`, `stralgolcslen` and
`stralgolcsidx` in `laserdisc.redis_6_2.*` fail on Redis 7.0 and later. Once all
your servers run Redis 7.0 or later, switch to `laserdisc.redis_7_0.*` and use
`lcs`, `lcslen` and `lcsidx` instead. `LCS` only compares the values at two
keys, so to compare two values directly, store them at keys first.

## Dragonfly

`laserdisc.dragonfly_2_0.*` has only what Dragonfly accepts. That leaves out, for
example, `MIGRATE`, `OBJECT`, `SWAPDB`, `FUNCTION` and most of `CLUSTER`. It also
adds Dragonfly's own commands: `prepend`, `saddex`, `fieldexpire`, `fieldttl`,
`stick`, `clthrottle`, `rm` and `repltakeover`.

Its rules for `ACL SETUSER` leave out `reset`, `hashedPassword` and
`removeHashedPassword`, which Dragonfly refuses. `shutdown` takes `now` or
`force` alone, as Dragonfly refuses them together or with `save` or `nosave`.

Some things still behave differently from Redis:

- `CLUSTER` only works when Dragonfly runs in cluster mode, with
  `--cluster_mode=emulated` or `--cluster_mode=yes`.
- `role` replies with Dragonfly's own type, which has each server's state and no
  replication offsets.
- `SORT` ignores `BY`, and `GET` gives an empty value for a missing key.
- `INFO` with the `default` or `everything` section replies with nothing. Use
  `info` with no section, or the `all` section.
- `ACL LOAD`, `ACL SAVE` and `CONFIG REWRITE` need Dragonfly to have been started
  with those files.
- `PUBSUB SHARDCHANNELS` and `PUBSUB SHARDNUMSUB` need cluster mode.
- `CLIENT TRACKING` isn't available, because Dragonfly only allows it with RESP3,
  and LaserDisc reads RESP2.

## Commands LaserDisc doesn't send

These commands aren't available on purpose, because they're meant for servers
talking to each other, because they would break or stall your connection, or
because they have no use on a connection shared by all your calls:

| Command | Why |
| --- | --- |
| `PSYNC`, `SYNC`, `REPLCONF` | They turn the connection into a replication link |
| `ASKING` | It's only for the command right after a cluster sends you to another node, which LaserDisc doesn't follow |
| `RESTORE-ASKING`, `CLUSTER SYNCSLOTS`, `XIDMPRECORD` | Servers send these to each other |
| `SCRIPT DEBUG` | It takes over the connection with an interactive debugger |
| `UNSUBSCRIBE`, `PUNSUBSCRIBE`, `SUNSUBSCRIBE` | Closing a [subscription](subscribing.md) ends it |
| `HELLO 3` | LaserDisc reads RESP2, so `hello` only asks for that |
| `AUTH`, `SELECT`, `HELLO` with `AUTH` | A connection the client opens again wouldn't have them: pass `credentials` and `database` when you [connect](client.md) |
| `QUIT` | Closing the client closes its connections |
| `DISCARD` | A transaction is sent whole, so there's never an open one to cancel |
| `RESET` | It would log out and change the database for everyone sharing the connection |
| `CLIENT REPLY` | Replies are matched to requests by their order, so a missing reply breaks the connection |
| Dragonfly's `SHRINK`, `ADDREPLICAOF` | They aren't documented, so what they take and reply can't be relied on |
| `DEBUG`, `PFDEBUG` | They can crash or stall a server, and their replies can change between versions |
