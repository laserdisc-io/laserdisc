# Commands

A command is a value of type `Protocol.Aux[A]`, where `A` is the type of its
reply. Building one doesn't send anything; you send it with a
[client](client.md).

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.all.*
import laserdisc.auto.*

val setA: Protocol.Aux[OK]             = set("a", 23)
val getA: Protocol.Aux[Option[PosInt]] = get[PosInt]("a")
```

`laserdisc.all.*` brings every command into scope. To import a single family
instead, use its object:

| Family | Import |
| --- | --- |
| Strings | `laserdisc.strings.*` |
| Keys | `laserdisc.keys.*` |
| Hashes | `laserdisc.hashmaps.*` |
| Lists | `laserdisc.lists.*`, and `laserdisc.lists.blocking.*` for blocking commands |
| Sets | `laserdisc.sets.*` |
| Sorted sets | `laserdisc.sortedsets.*` |
| Geospatial | `laserdisc.geo.*` |
| HyperLogLog | `laserdisc.hyperloglog.*` |
| Publish | `laserdisc.publish.*` |
| Transactions | `laserdisc.transaction.*` |
| Connection | `laserdisc.connection.*` |
| Server | `laserdisc.server.*` |
| Cluster | `laserdisc.cluster.*` |

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

## Your own types

To store values of your own types, give LaserDisc a `Show` to write them and a
`Read` to read them back:

```scala mdoc:compile-only
import laserdisc.*
import laserdisc.all.*
import laserdisc.auto.*

final case class UserId(value: Long)

implicit val userIdShow: Show[UserId]       = Show.instance(_.value.toString)
implicit val userIdRead: Read[Bulk, UserId] = Read[Bulk, Long].map(UserId(_))

val save = set("user", UserId(42L))
val load = get[UserId]("user")
```

`Read.instance` lets you fail with an error when a value doesn't fit, and
`Show.instance` turns a value into the string Redis stores.
