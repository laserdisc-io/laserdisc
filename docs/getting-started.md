# Getting started

## Dependency

To talk to a server, add the fs2 client:

```scala
libraryDependencies += "io.laserdisc" %%% "laserdisc-fs2" % "@VERSION@"
```

If you only need the commands and the wire format, for example to write your
own client, add the core module instead:

```scala
libraryDependencies += "io.laserdisc" %%% "laserdisc-core" % "@VERSION@"
```

`%%%` picks the right artifact for the JVM, Scala.js or Scala Native. On the
JVM alone, `%%` works too.

## Modules

- `laserdisc-core` holds the commands, the types of their arguments and
  replies, and the Redis wire format.
- `laserdisc-fs2` holds `RedisClient`, which sends commands to a server. It
  depends on `laserdisc-core`.
- `laserdisc-circe` reads and writes values as JSON. See [JSON with circe](circe.md).
- `laserdisc-cli` is an interactive prompt. See [Command line](cli.md).

## Versioning

LaserDisc follows early semantic versioning.

Upgrades within a series keep binary compatibility: code built against `0.8.0`
runs against `0.8.1` without recompiling.

Upgrades across a series, from `0.8.x` to `0.9.0`, may need recompiling and
changes to your code.

While the major version is `0`, expect the API to change between series.

## Next steps

- [Commands](commands.md) shows how to build commands and read their replies.
- [Client](client.md) shows how to connect and send commands.
