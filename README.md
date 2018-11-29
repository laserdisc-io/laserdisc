## LaserDisc

[![Codacy Badge](https://api.codacy.com/project/badge/Grade/d548a2d7facc4e42b422940dbf5c8382)](https://app.codacy.com/app/barambani/laserdisc?utm_source=github.com&utm_medium=referral&utm_content=laserdisc-io/laserdisc&utm_campaign=Badge_Grade_Dashboard)
[![Build Status](https://travis-ci.org/laserdisc-io/laserdisc.svg?branch=master)](https://travis-ci.org/laserdisc-io/laserdisc)
[![codecov.io](https://codecov.io/github/laserdisc-io/laserdisc/coverage.svg?branch=master)](https://codecov.io/github/laserdisc-io/laserdisc?branch=master)
[![Join the chat at https://gitter.im/laserdisc-io/laserdisc](https://badges.gitter.im/laserdisc-io/laserdisc.svg)](https://gitter.im/laserdisc-io/laserdisc?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<br>
[![Maven Central](https://img.shields.io/maven-central/v/io.laserdisc/laserdisc-core_2.12.svg?label=laserdisc-core&colorB=orange)](https://index.scala-lang.org/laserdisc-io/laserdisc/laserdisc-core)
[![Maven Central](https://img.shields.io/maven-central/v/io.laserdisc/laserdisc-core_2.12.svg?label=laserdisc-core-docs&colorB=orange)](https://javadoc.io/doc/io.laserdisc/laserdisc-core_2.12)
[![Maven Central](https://img.shields.io/maven-central/v/io.laserdisc/laserdisc-fs2_2.12.svg?label=laserdisc-fs2&colorB=blue)](https://index.scala-lang.org/laserdisc-io/laserdisc/laserdisc-fs2)
[![Maven Central](https://img.shields.io/maven-central/v/io.laserdisc/laserdisc-core_2.12.svg?label=laserdisc-fs2-docs&colorB=blue)](https://javadoc.io/doc/io.laserdisc/laserdisc-fs2_2.12)
[![Scala.js](http://scala-js.org/assets/badges/scalajs-0.6.17.svg)](http://scala-js.org)

LaserDisc is a(nother) Scala driver for [Redis](https://redis.io/), written in Scala from the ground up.

It differentiates itself from the others for having a core layer, which is made up of all the supported Redis commands
and the Redis Serialization Protocol ([RESP](https://redis.io/topics/protocol)), that is strongly typed and which makes
heavy use of [shapeless](https://github.com/milessabin/shapeless) and [refined](https://github.com/fthomas/refined) to
achieve this. It's also worth noting that the core - in order to be built on scala 2.11.x - makes use of
[Typelevel's Scala 2.11](https://typelevel.org/scala) fork, since it requires the enhancements on implicit heuristics. Finally, it also provides an implementation of RESP built using
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

### Interoperability modules

Support for existing libraries is available via dedicated dependencies.

#### [Circe](https://circe.github.io/circe/)

When an `io.circe.Decoder[A]` and a `io.circe.Encoder[A]` are implicilty available,
instances of `Show[A]` and `Read[NonNullBulkString, A]` can be derived for free,
just add the following in your `build.sbt`:

```
libraryDependecies += "io.laserdisc" %% "laserdisc-circe" % latestVersion 
```

then, to make use of them, at call site it should be sufficient to just:

```scala
import laserdisc.interop.circe._
```

*Note*: the derived `Show[A]` instance uses the most compact string representation
of the JSON data structure, i.e. no spacing is used

### Example usage
With a running Redis instance on `localhost:6379` try running the following:
```scala
import java.nio.channels.AsynchronousChannelGroup
import java.util.concurrent.Executors

import cats.effect._
import cats.syntax.apply._
import cats.syntax.functor._
import fs2.Stream
import laserdisc._
import laserdisc.auto._
import laserdisc.fs2._
import log.effect.fs2.Fs2LogWriter

import scala.concurrent.ExecutionContext

object Main extends IOApp.WithContext {

  override final protected val executionContextResource: Resource[SyncIO, ExecutionContext] =
    MkResource(SyncIO(ExecutionContext.fromExecutorService(Executors.newWorkStealingPool())))
      .widenRight[ExecutionContext]

  private[this] final val asynchronousChannelGroupResource: Resource[IO, AsynchronousChannelGroup] =
    MkResource(IO(AsynchronousChannelGroup.withThreadPool(Executors.newFixedThreadPool(2))))

  override final def run(args: List[String]): IO[ExitCode] =
    Stream.resource(asynchronousChannelGroupResource).flatMap { implicit acg =>
      Fs2LogWriter.consoleLogStream[IO].flatMap { implicit logger =>
        RedisClient[IO](Set(RedisAddress("localhost", 6379))).evalMap { client =>
          client.send2(strings.set("a", 23), strings.get[PosInt]("a")).flatMap {
            case (Right(OK), Right(Some(getResponse))) if getResponse.value == 23 =>
              logger.info("yay!")
            case other =>
              logger.error(s"something went terribly wrong $other") *>
                IO.raiseError(new RuntimeException("boom"))
          }
        }
      }
    }
    .compile
    .drain
    .as(ExitCode.Success)
}
```

This should produce an output similar to the following one:
```
[info] Running Main
[info] - [ForkJoinPool-3-worker-2] Starting connection
[info] - [ForkJoinPool-3-worker-2] Server available for publishing: localhost:6379
[debug] - [ForkJoinPool-3-worker-5] sending Array(BulkString(SET),BulkString(a),BulkString(23))
[debug] - [ForkJoinPool-3-worker-0] receiving SimpleString(OK)
[debug] - [ForkJoinPool-3-worker-1] sending Array(BulkString(GET),BulkString(a))
[debug] - [ForkJoinPool-3-worker-5] receiving BulkString(23)
[info] - [ForkJoinPool-3-worker-2] yay!
[info] - [ForkJoinPool-3-worker-2] Shutting down connection
[info] - [ForkJoinPool-3-worker-0] Connection terminated: Right(())
```

## License

LaserDisc is licensed under the **[MIT License](LICENSE)** (the "License"); you may not use this software except in
compliance with the License.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
