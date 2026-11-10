# LaserDisc

[![Continuous Integration](https://github.com/laserdisc-io/laserdisc/actions/workflows/ci.yml/badge.svg)](https://github.com/laserdisc-io/laserdisc/actions/workflows/ci.yml)
[![Known Vulnerabilities](https://snyk.io/test/github/laserdisc-io/laserdisc/badge.svg?targetFile=build.sbt)](https://snyk.io/test/github/laserdisc-io/laserdisc?targetFile=build.sbt)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

[![laserdisc-core Scala version support](https://index.scala-lang.org/laserdisc-io/laserdisc/laserdisc-core/latest-by-scala-version.svg?targetType=Js)](https://index.scala-lang.org/laserdisc-io/laserdisc/laserdisc-core)

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
| `laserdisc-core` | Commands and the Redis wire format | ✓ | ✓ | ✓ |
| `laserdisc-fs2` | A client built on [fs2](https://fs2.io/) and [Cats Effect](https://typelevel.org/cats-effect/) | ✓ | ✓ | ✓ |
| `laserdisc-circe` | Store and read values as JSON with [circe](https://circe.github.io/circe/) | ✓ | ✓ | ✓ |
| `laserdisc-cli` | An interactive prompt, like `redis-cli` | ✓ | ✓ | ✓ |

## Usage

```scala
libraryDependencies += "io.laserdisc" %%% "laserdisc-fs2" % latestVersion
```

With Redis running on `localhost:6379`:

```scala
import cats.effect.{IO, IOApp}
import log.effect.LogWriter
import log.effect.fs2.SyncLogWriter

import laserdisc.*
import laserdisc.redis_8_10.*
import laserdisc.auto.*
import laserdisc.fs2.*

object Main extends IOApp.Simple {
  implicit val logWriter: LogWriter[IO] = SyncLogWriter.consoleLog[IO]

  override val run: IO[Unit] =
    RedisClient[IO].to("localhost", 6379).use { client =>
      client.send(set("a", 23), get[PosInt]("a")).flatMap {
        case (Right(OK), Right(Some(a))) => IO.println(s"a is ${a.value}")
        case other                       => IO.raiseError(new RuntimeException(s"unexpected reply: $other"))
      }
    }
}
```

## Documentation

The full documentation is at
[www.laserdisc.io](https://www.laserdisc.io/).
It covers commands and their argument types, reading replies into your own
types, the client, JSON with circe, the command line prompt, and the
differences between platforms.

## Versioning

LaserDisc follows early semantic versioning. Upgrades within a series, such as
`0.8.0` to `0.8.1`, keep binary compatibility. Upgrades across a series, such
as `0.8.x` to `0.9.0`, may need changes to your code.

## Building

To build LaserDisc or run its tests, see
[Contributing](https://www.laserdisc.io/contributing.html).

## License

LaserDisc is released under the [MIT License](LICENSE).
