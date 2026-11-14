# LaserDisc

LaserDisc is a Scala client for [Redis](https://redis.io/) and for servers that
speak the same protocol, such as [Valkey](https://valkey.io/),
[KeyDB](https://docs.keydb.dev/) and [Dragonfly](https://www.dragonflydb.io/).

Every command takes arguments whose types only accept valid values, so a key
can't be empty and a port can't be out of range. Mistakes in literal values are
caught when your code compiles, and you choose the type each reply is read
into.

LaserDisc works with Scala 2.12, 2.13 and 3.9 or newer, on Java 17 or newer.

| Module | What it gives you | JVM | Scala.js | Scala Native |
| --- | --- | --- | --- | --- |
| `laserdisc-core` | The commands, and how they and their replies are written on the wire | ✓ | ✓ | ✓ |
| `laserdisc-fs2` | A client built on [fs2](https://fs2.io/) and [Cats Effect](https://typelevel.org/cats-effect/) | ✓ | ✓ | ✓ |
| `laserdisc-circe` | Store and read values as JSON with [circe](https://circe.github.io/circe/) | ✓ | ✓ | ✓ |
| `laserdisc-cli` | An interactive prompt, like `redis-cli` | ✓ | ✓ | ✓ |

To add LaserDisc to your build and connect for the first time, see
[Getting started](getting-started.md). With Redis running on `localhost:6379`,
this sets a key and reads it back:

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

## Why the name

"A LaserDisc" is an anagram of "Scala Redis". LaserDiscs were also the first
optical discs to offer random access, which suits a key-value store. More on
[Wikipedia](https://en.wikipedia.org/wiki/LaserDisc).
