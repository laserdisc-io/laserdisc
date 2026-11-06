# Commands

A command is a value of type `Protocol.Aux[A]`, where `A` is the type of its
reply. Building one doesn't send anything; you send it with a
[client](client.md).

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*

val setA: Protocol.Aux[OK]             = set("a", 23)
val getA: Protocol.Aux[Option[PosInt]] = get[PosInt]("a")
```

`laserdisc.redis_8_10.*` brings every command of Redis 8.10 into scope. Use the
import for your server: see [Your server's version](#your-servers-version).

To import a single family instead, add its name:

| Family | Import |
| --- | --- |
| Strings | `laserdisc.redis_8_10.strings.*` |
| Streams | `laserdisc.redis_8_10.streams.*` |
| Keys | `laserdisc.redis_8_10.keys.*` |
| Hashes | `laserdisc.redis_8_10.hashmaps.*` |
| Arrays | `laserdisc.redis_8_10.arrays.*` |
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

## Your server's version

Each server and version has its own import, with only what that server has:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.auto.*
import laserdisc.valkey_8_1.*

val setIfOne = set("a", 2, stringtypes.ifEq(1))
```

A command or option your server doesn't have won't compile. For example,
`stringtypes.ifEq` arrived in Valkey 8.1 and Redis 8.4, so the same line doesn't
compile with `laserdisc.redis_8_2.*`.

| Server | Import |
| --- | --- |
| Redis 6.2 to 8.10 | `laserdisc.redis_6_2.*`, `laserdisc.redis_7_0.*`, `laserdisc.redis_7_2.*`, `laserdisc.redis_7_4.*`, `laserdisc.redis_8_0.*`, `laserdisc.redis_8_2.*`, `laserdisc.redis_8_4.*`, `laserdisc.redis_8_6.*`, `laserdisc.redis_8_8.*`, `laserdisc.redis_8_10.*` |
| Valkey 7.2 to 9.1 | `laserdisc.valkey_7_2.*`, `laserdisc.valkey_8_0.*`, `laserdisc.valkey_8_1.*`, `laserdisc.valkey_9_0.*`, `laserdisc.valkey_9_1.*` |
| KeyDB | `laserdisc.redis_6_2.*` |

Each import covers every patch release of its version: `laserdisc.redis_7_2.*`
is for Redis 7.2.0 and later 7.2 releases.

## Commands laserdisc doesn't send

These commands aren't available on purpose, because they're meant for servers
talking to each other, or because they would break or stall your connection:

| Command | Why |
| --- | --- |
| `PSYNC`, `SYNC`, `REPLCONF` | They turn the connection into a replication link |
| `ASKING`, `RESTORE-ASKING`, `CLUSTER SYNCSLOTS`, `XIDMPRECORD` | Servers send these to each other |
| `SCRIPT DEBUG` | It takes over the connection with an interactive debugger |
| `DEBUG`, `PFDEBUG` | They can crash or stall a server, and their replies can change between versions |

## Arguments

Arguments use types that only accept valid values. For example, a `Key` is a
non-empty string without control characters, a `PosInt` is an `Int` greater
than zero, and a `Port` is an `Int` between 1 and 65535.

With `laserdisc.auto.*` imported, you can pass literal values directly. They're
checked when your code compiles, so this doesn't compile:

```scala
set("", 23) // an empty key
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

## Replies

Commands that return a value let you choose its type. `get[PosInt]("a")` reads
the value at `a` as a `PosInt`; `get[String]("a")` reads it as a `String`.

If the reply can't be read as that type, the result is an error rather than a
wrong value. If the key doesn't exist, `get` gives `None`.

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

`hgetall` also reads a hash as a `Map`, like `hgetall[Map[Key, String]]`, or as
a list of pairs, like `hgetall[Seq[(Key, Int)]]`.

`hmget` reads the fields you name into a tuple, in the same order. Use an
`Option` for a field that may be missing; any other missing field is an error
that says which one:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*

val fields  = OneOrMoreKeys.unsafeFrom(List(Key("age"), Key("email")))
val profile = hmget[Int *: Option[String] *: EmptyTuple]("user:1", fields)
```

`mget` works the same way for keys.

## Scripts

A script can return anything, so `eval`, `evalsha` and `fcall` take two types:
the kind of reply the script sends, and the type to read it as. The keys and the
arguments are the script's `KEYS` and `ARGV`:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*

val keys = OneOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
val args = OneOrMore.unsafeFrom(List[Value](1, "two"))

// a number
val count: Protocol.Aux[Long] = eval[Num, Long]("return #KEYS + #ARGV", keys, args)
// a table of numbers
val counts: Protocol.Aux[Seq[Long]] = eval[Arr.Of[Num], Seq[Long]]("return {#KEYS, #ARGV}", keys, args)
// a string, or nil
val value: Protocol.Aux[Option[String]] = eval[OrNullBulk[Bulk], Option[String]]("return redis.call('GET', KEYS[1])", keys)
// always nil
val done: Protocol.Aux[Unit] = evalargs[NullBulk, Unit]("redis.log(redis.LOG_NOTICE, ARGV[1])", args)
```

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
  `Maybe` of its own.
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

## Your own types

To store values of your own types, give LaserDisc a `Show` to write them and a
`Read` to read them back:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*

final case class UserId(value: Long)

implicit val userIdShow: Show[UserId]       = Show.instance(_.value.toString)
implicit val userIdRead: Read[Bulk, UserId] = Read[Bulk, Long].map(UserId(_))

val save = set("user", UserId(42L))
val load = get[UserId]("user")
```

`Read.instance` lets you fail with an error when a value doesn't fit, and
`Show.instance` turns a value into the string Redis stores.
