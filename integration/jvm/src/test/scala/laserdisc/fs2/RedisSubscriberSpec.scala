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
package fs2

import cats.effect.IO
import laserdisc.auto.*
import laserdisc.protocol.Feed.{Message, MonitorEvent, PatternMessage, ShardMessage}
import laserdisc.protocol.Feed

import scala.concurrent.duration.*

final class RedisRedisSubscriberSpec extends RedisSubscriberSpec(LiveServers.port(6379), "redis") {
  since[Redis7_0]("a redis client is told which keys it read changed")(v => tracking(v.client))
  since[Redis7_0]("a redis client reads messages of shard channels")(v => shards(v))
  test("a redis client is told about a wrong password")(wrongPassword())
}
final class KeyDbRedisSubscriberSpec extends RedisSubscriberSpec(LiveServers.port(6380), "keyDb") {
  test("a keyDb client is told which keys it read changed")(tracking(laserdisc.redis_6_2.client))
  test("a keyDb client is told about a wrong password")(wrongPassword())
}
final class ValkeyRedisSubscriberSpec extends RedisSubscriberSpec(LiveServers.port(6381), "valkey") {
  since[Valkey7_2]("a valkey client is told which keys it read changed")(v => tracking(v.client))
  since[Valkey7_2]("a valkey client reads messages of shard channels")(v => shards(v))
  test("a valkey client is told about a wrong password")(wrongPassword())
}
final class DragonflyRedisSubscriberSpec
    extends RedisSubscriberSpec(LiveServers.port(6382), "dragonfly", laserdisc.dragonfly_2_0.connection) {
  test("a dragonfly client reads messages of shard channels")(shards(laserdisc.dragonfly_2_0))
}

// `connection` is the server's own HELLO, as Dragonfly's reply has no modules
abstract class RedisSubscriberSpec(p: Port, dest: String, connection: protocol.ConnectionBaseP = laserdisc.redis_6_2.connection)
    extends LaserdiscFs2Suite(p) {
  private val publisher = laserdisc.redis_6_2

  protected final def run[A](io: IO[A]): A = io.timeout(10.seconds).unsafeRunSync()(using runtime)

  private def send[A](protocol: Protocol[A]): A =
    run(client.send(protocol)).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  private def channels(names: String*) = OneOrMoreKeys.unsafeFrom(names.toList.map(Key.unsafeFrom(_)))

  protected final def shards(v: laserdisc.protocol.PublishShardP): Unit = {
    val received = RedisSubscriber[IO].to("127.0.0.1", p, v.ssubscribe[String](channels("sub:s"))).use { messages =>
      IO(send(v.spublish(Key("sub:s"), "m"))) *> messages.take(1).compile.toList
    }
    assertEquals(run(received), List(ShardMessage(Key("sub:s"), "m")))
  }

  protected final def tracking(c: laserdisc.protocol.ClientCommands): Unit = {
    val options = laserdisc.redis_6_2.servertypes.trackingOptions
    val checked = RedisSubscriber[IO].identified("127.0.0.1", p, c.invalidations).use { case (id, notices) =>
      RedisClient[IO].to("127.0.0.1", p).use { own =>
        def ok[A](protocol: Protocol[A]) = own.send(protocol).flatMap(IO.fromEither)
        for {
          _     <- ok(publisher.set(Key("sub:t"), "1"))
          _     <- ok(c.tracking(options.redirect(id)))
          redir <- ok(c.getredir)
          info  <- ok(c.trackinginfo)
          _     <- ok(publisher.get[String](Key("sub:t")))
          _     <- IO(send(publisher.set(Key("sub:t"), "2")))
          seen  <- notices.take(1).compile.toList
          _     <- ok(c.trackingoff)
          off   <- ok(c.getredir)
          _     <- ok(c.tracking(options.redirect(id).optin))
          _     <- own.send(c.caching(true), publisher.get[String](Key("sub:t")))
          _     <- IO(send(publisher.set(Key("sub:t"), "3")))
          next  <- notices.take(1).compile.toList
          _     <- ok(c.trackingoff)
          _     <- ok(c.tracking(options.redirect(id).bcast.prefix(Key("sub:b:")).noloop))
          bcast <- ok(c.trackinginfo)
          _     <- IO(send(publisher.set(Key("sub:b:1"), "1")))
          third <- notices.take(1).compile.toList
          _     <- ok(c.trackingoff)
        } yield (id, redir, info, seen, off, next, bcast, third)
      }
    }
    val (id, redir, info, seen, off, next, bcast, third) = run(checked)
    val changed                                          = Feed.Invalidation.Keys(Seq(Key("sub:t")))
    assertEquals(redir, Some(id))
    assertEquals(info.redirect, Some(id))
    assert(info.flags.contains("on"))
    assertEquals(seen, List[Feed.Invalidation](changed))
    assertEquals(off, None)
    assertEquals(next, List[Feed.Invalidation](changed))
    assertEquals(bcast.prefixes, Seq("sub:b:"))
    assert(bcast.flags.contains("bcast"))
    assertEquals(third, List[Feed.Invalidation](Feed.Invalidation.Keys(Seq(Key("sub:b:1")))))
  }

  protected final def wrongPassword(): Unit = {
    val attempt = RedisSubscriber[IO]
      .to("127.0.0.1", p, Key("wrong"), publisher.subscribe[String](channels("sub:p")))
      .use(_ => IO.unit)
      .attempt
    assert(run(attempt).isLeft)
  }

  test(s"a $dest client reads the messages of channels, as the type it asked for, with the server having confirmed them all") {
    val received = RedisSubscriber[IO].to("127.0.0.1", p, publisher.subscribe[Int](channels("sub:a", "sub:b"))).use { messages =>
      IO(send(publisher.publish(Key("sub:a"), 1))) *>
        IO(send(publisher.publish(Key("sub:b"), 2))) *>
        messages.take(2).compile.toList
    }
    assertEquals(run(received), List(Message(Key("sub:a"), 1), Message(Key("sub:b"), 2)))
  }

  test(s"a $dest client reads the messages of channels that match a pattern") {
    val patterns = OneOrMore.unsafeFrom(List(GlobPattern("sub:p*")))
    val received = RedisSubscriber[IO].to("127.0.0.1", p, publisher.psubscribe[String](patterns)).use { messages =>
      IO(send(publisher.publish(Key("sub:p1"), "m"))) *> messages.take(1).compile.toList
    }
    assertEquals(run(received), List(PatternMessage(GlobPattern("sub:p*"), Key("sub:p1"), "m")))
  }

  test(s"a $dest client's stream fails on a message it can't read as the type it asked for") {
    val attempt = RedisSubscriber[IO]
      .to("127.0.0.1", p, publisher.subscribe[Int](channels("sub:bad")))
      .use { messages =>
        IO(send(publisher.publish(Key("sub:bad"), "not a number"))) *> messages.take(1).compile.toList
      }
      .attempt
    assert(run(attempt).isLeft)
  }

  test(s"a $dest client says hello, names its connection and reads what the server says") {
    val named = RedisClient[IO].to("127.0.0.1", p).use { own =>
      own.send(connection.hello(ConnectionName("ld-hello"))).flatMap(IO.fromEither) product own
        .send(publisher.client.getname)
        .flatMap(IO.fromEither)
    }
    val (info, name) = run(named)
    assertEquals(info.protocol, 2L)
    assert(info.id.value > 0L)
    assertEquals(name.map(_.value), Some("ld-hello"))
  }

  test(s"a $dest client is no longer subscribed once closed") {
    val names = channels("sub:closed")
    run(RedisSubscriber[IO].to("127.0.0.1", p, publisher.subscribe[String](names)).use { _ =>
      IO(assertEquals(send(publisher.pubsub.numsub(names)), Seq(Key("sub:closed") -> NonNegInt(1))))
    })
    def waitForIt(attempts: Int): Unit =
      if (send(publisher.pubsub.numsub(names)).exists(_._2.value == 0)) ()
      else if (attempts == 0) fail("still subscribed")
      else {
        Thread.sleep(50L)
        waitForIt(attempts - 1)
      }
    waitForIt(100)
  }

  test(s"a $dest client monitors the commands the server receives") {
    val received = RedisSubscriber[IO].to("127.0.0.1", p, publisher.monitor).use { events =>
      IO(send(publisher.set(Key("sub:monitored"), "a \"quoted\" value"))) *>
        events
          .collectFirst { case seen @ MonitorEvent(_, _, _, command) if command.lift(1).contains("sub:monitored") => seen }
          .compile
          .toList
    }
    val event = run(received).headOption.getOrElse(fail("expected an event"))
    assertEquals(event.command.map(_.toUpperCase).head, "SET")
    assertEquals(event.command.drop(1), Seq("sub:monitored", "a \"quoted\" value"))
    assert(event.time > BigDecimal(0))
  }
}

// a server of a few lines, for what a real one can't be told to do
