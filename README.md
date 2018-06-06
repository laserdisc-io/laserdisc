## LaserDisc

[![Build Status](https://travis-ci.org/laserdisc-io/laserdisc.svg?branch=master)](https://travis-ci.org/laserdisc-io/laserdisc)
[![Scala.js](http://scala-js.org/assets/badges/scalajs-0.6.17.svg)](http://scala-js.org)
[![Join the chat at https://gitter.im/laserdisc-io/laserdisc](https://badges.gitter.im/laserdisc-io/laserdisc.svg)](https://gitter.im/laserdisc-io/laserdisc?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Latest core version](https://index.scala-lang.org/laserdisc-io/laserdisc/laserdisc-core/latest.svg?color=orange&v=1)](https://index.scala-lang.org/laserdisc-io/laserdisc/laserdisc-core)
[![Javadocs](https://javadoc.io/badge/io.laserdisc/laserdisc-core_2.12.svg?color=orange&label=laserdisc-core docs)](https://javadoc.io/doc/io.laserdisc/laserdisc-core_2.12)
[![Latest fs2 version](https://index.scala-lang.org/laserdisc-io/laserdisc/laserdisc-fs2/latest.svg?color=blue&v=1)](https://index.scala-lang.org/laserdisc-io/laserdisc/laserdisc-fs2)
[![Javadocs](https://javadoc.io/badge/io.laserdisc/laserdisc-fs2_2.12.svg?color=blue&label=laserdisc-fs2 docs)](https://javadoc.io/doc/io.laserdisc/laserdisc-fs2_2.12)

LaserDisc is a(nother) Scala driver for [Redis](https://redis.io/), written in Scala from the ground up.

It differentiates itself from the others for having a core layer, which is made up of all the supported Redis commands
and the Redis Serialization Protocol ([RESP](https://redis.io/topics/protocol)), that is strongly typed and which makes
heavy use of [shapeless](https://github.com/milessabin/shapeless) and [refined](https://github.com/fthomas/refined) to
achieve this. It's also worth noting that the core - in order to be built - makes use of
[Typelevel's Scala 2.12](https://typelevel.org/scala) fork, since it requires the enhancements on implicit heuristics
and the surfacing of literal types. Finally, it also provides an implementation of RESP built using
[scodec](http://scodec.org/).

On top of this, one or more clients can be implemented. The only one currently available out of the box is built using
[fs2](https://functional-streams-for-scala.github.io/fs2/)/[cats effect](https://typelevel.org/cats-effect/) but
more competing implementations can be added with limited effort. This implementation has found great inspiration from
the excellent [fs2-kafka](https://github.com/Spinoco/fs2-kafka/) library.

### *Disclaimer*

:warning: Development is very much early stages and subject to frequent and breaking changes. :warning:

What's there:
- [x] Codecs for Redis' RESP wire format
- [x] Fully-fledged protocol encapsulating request/response pairs for (almost) all Redis [commands](https://redis.io/commands)
- [x] Initial version of single-node Redis client. Lots of improvements needed
- [x] Minimal CLI

What's missing:
- [ ] Everything else :)

### Why the name

Two reasons:
1. "A LaserDisc" is an anagram for "Scala Redis"
2. LaserDiscs were invented in 1978 (same year I was born) and were so cool (and foundational, more on [Wikipedia](https://en.wikipedia.org/wiki/LaserDisc))

### Getting Started

LaserDisc is currently available for Scala 2.11 and 2.12 on the JVM.

Its core (protocol commands and RESP wire format) is also available for [Scala.JS](http://www.scala-js.org/).

To add LaserDisc as a dependency to your project just add the following to your `build.sbt`:
```
libraryDependencies += "io.laserdisc" %% "laserdisc-fs2" % latestVersion
```

If you only need protocols (i.e. Redis commands and RESP wire format), you may simply add:
```
libraryDependencies += "io.laserdisc" %% "laserdisc-core" % latestVersion
```

### Example usage
With a running Redis instance on `localhost:6379` try running the following:
```scala
import java.nio.channels.AsynchronousChannelGroup
import java.util.concurrent.Executors.newFixedThreadPool

import cats.effect.IO
import cats.syntax.apply._
import fs2.{Scheduler, Stream, StreamApp}
import laserdisc._
import laserdisc.auto._
import laserdisc.fs2._

import scala.concurrent.ExecutionContext

object Main extends StreamApp[IO] {

  implicit final val ec: ExecutionContext          = ExecutionContext.global
  implicit final val asg: AsynchronousChannelGroup = AsynchronousChannelGroup.withThreadPool(newFixedThreadPool(2))
  implicit final val logger: Logger[IO]            = new Logger[IO] {
    override def log(level: Logger.Level, msg: => String, maybeT: Logger.MaybeT): IO[Unit] = IO {
      println(s">>> $msg")
    }
  }

  override final def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, StreamApp.ExitCode] =
    Scheduler[IO](corePoolSize = 1).flatMap { implicit scheduler =>
      RedisClient[IO](Set(RedisAddress("localhost", 6379))).evalMap { client =>
        client.send2(string.set("a", 23), string.get[PosInt]("a")).flatMap {
          case (Right("OK"), Right(Some(getResponse))) if getResponse.value == 23 => logger.info("yay!") *> IO.pure(StreamApp.ExitCode.Success)
          case other => logger.error(s"something went terribly wrong $other") *> IO.raiseError(new RuntimeException("boom"))
        }
      }
    }
    .onFinalize(IO(asg.shutdown))
}
```

This should produce the following output:
```bash
>>> Starting connection
>>> Server available for publishing: localhost:6379
>>> sending Array(BulkString(SET),BulkString(a),BulkString(23))
>>> receiving SimpleString(OK)
>>> sending Array(BulkString(GET),BulkString(a))
>>> receiving BulkString(23)
>>> yay!
>>> Shutting down connection
>>> Connection terminated: Left(java.nio.channels.ShutdownChannelGroupException)
```

## License

LaserDisc is licensed under the **[MIT License](LICENSE)** (the "License"); you may not use this software except in
compliance with the License.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
