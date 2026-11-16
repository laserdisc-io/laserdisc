# Getting started

## Add LaserDisc to your build

To talk to a server, add the client. On the JVM:

```scala
libraryDependencies += "io.laserdisc" %% "laserdisc-fs2" % "@VERSION@"
```

In a Scala.js or Scala Native project, use `%%%` instead of `%%`, so that sbt
picks the artifact for your platform.

If you only need the commands, for example to write your own client, add
`laserdisc-core` instead. The other modules are listed on the
[home page](index.md).

The examples in these pages use Scala 3's `*` imports, such as
`import laserdisc.*`. On Scala 2, add `scalacOptions += "-Xsource:3"` to your
build to use them as they are, or write `_` in their place. On Scala 2.12,
also add `"-Ypartial-unification"`, which cats needs there too.

## Run a server

If you don't have one at hand, start Redis with Docker:

```sh
docker run --rm -p 6379:6379 redis
```

## Connect and send commands

```scala mdoc:compile-only
import cats.effect.{IO, IOApp}
import log.effect.{LogLevels, LogWriter}
import log.effect.fs2.SyncLogWriter

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

object Main extends IOApp.Simple {
  // logs the client's connections to the console: leave it out to log nothing
  implicit val logWriter: LogWriter[IO] = SyncLogWriter.consoleLogUpToLevel(LogLevels.Info)

  override val run: IO[Unit] =
    RedisClient[IO].to("localhost", 6379).use { client =>
      // both commands go to the server together, and each reply comes back as a Right, or a Left if that command failed
      client.send(set("a", 23), get[PosInt]("a")).flatMap {
        // get gives None when the key doesn't exist, and .value turns the PosInt back into an Int
        case (Right(OK), Right(Some(a))) => IO.println(s"a is ${a.value}")
        case other                       => IO.raiseError(new RuntimeException(s"unexpected reply: $other"))
      }
    }
}
```

What each import gives you:

- `laserdisc.*` is the argument and reply types, such as `Key` and `PosInt`.
- `laserdisc.redis_8_10.*` is the commands of Redis 8.10. Import the version of
  your server, or the oldest one you run: see
  [The version of your server](commands.md#the-version-of-your-server).
- `laserdisc.auto.*` turns literals into argument types, checked when your code
  compiles: `"localhost"` is passed as a `Host`, `6379` as a `Port` and `"a"` as
  a `Key`.
- `laserdisc.fs2.*` is the client.

The `LogWriter` comes from [log-effect](https://github.com/laserdisc-io/log-effect),
which `laserdisc-fs2` depends on. See [Logging](client.md#logging).

## Versioning

LaserDisc follows early semantic versioning.

Upgrades within a series keep binary compatibility: code built against `0.8.0`
runs against `0.8.1` without recompiling.

Upgrades across a series, from `0.8.x` to `0.9.0`, may need recompiling and
changes to your code.

## Next steps

- [Commands](commands.md) shows how to build commands and read their replies.
- [Client](client.md) shows how to connect, send commands and handle errors.
- [Subscribing](subscribing.md) shows how to receive messages from channels, and
  to know when keys change.
- [JSON with circe](circe.md) shows how to store and read JSON.
- [Command line](cli.md) shows how to send commands from a prompt.
- [Platform notes](platform-notes.md) says what differs between Scala versions,
  and on Scala.js and Scala Native.
