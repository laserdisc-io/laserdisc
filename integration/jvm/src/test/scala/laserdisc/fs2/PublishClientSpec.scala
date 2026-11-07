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

final class RedisPublishClientSpec extends PublishClientSpec(6379, "redis") {
  test("a redis client reads shard channels, and publishes to them, with the commands of Redis 7.0")(shards(redis_7_0))
}
final class KeyDbPublishClientSpec  extends PublishClientSpec(6380, "keyDb")
final class ValkeyPublishClientSpec extends PublishClientSpec(6381, "valkey") {
  test("a valkey client reads shard channels, and publishes to them, with the commands of Valkey 7.2")(shards(valkey_7_2))
}

final class DragonflyPublishClientSpec extends PublishClientSpec(6382, "dragonfly")

abstract class PublishClientSpec(p: Port, dest: String) extends LaserdiscFs2Suite(p) {
  private def send[A](protocol: Protocol.Aux[A]): A =
    client.send(protocol).unsafeRunSync()(using runtime).fold(e => fail(s"$dest replied with an error: ${e.getMessage}", e), identity)

  // a subscriber that stays quiet, for the others to see
  private def subscribed[A](command: String, name: String)(f: =>A): A =
    RedisClient[IO]
      .to("127.0.0.1", p)
      .use(subscriber => subscriber.send(Protocol(command, name :: Nil).using(protocol.RESPRead.raw)) *> IO(f))
      .unsafeRunSync()(using runtime)

  test(s"a $dest client sees channels and patterns with subscribers, and publishes to them") {
    import laserdisc.redis_6_2.*

    val names = OneOrMoreKeys.unsafeFrom(List(Key("pub:c"), Key("pub:none")))

    assertEquals(send(pubsub.channels(GlobPattern("pub:*"))), Seq.empty[Key])
    assertEquals(send(publish(Key("pub:c"), "m")), NonNegInt(0))

    subscribed("SUBSCRIBE", "pub:c") {
      assert(send(pubsub.channels).contains(Key("pub:c")))
      assertEquals(send(pubsub.channels(GlobPattern("pub:*"))), Seq(Key("pub:c")))
      assertEquals(send(pubsub.numsub(names)), Seq(Key("pub:c") -> NonNegInt(1), Key("pub:none") -> NonNegInt(0)))
      assertEquals(send(publish(Key("pub:c"), "m")), NonNegInt(1))
    }

    val patterns = send(pubsub.numpat)
    subscribed("PSUBSCRIBE", "pub:p*") {
      assertEquals(send(pubsub.numpat).value, patterns.value + 1)
      assertEquals(send(publish(Key("pub:p1"), "m")), NonNegInt(1))
    }
  }

  protected final def shards(v: Redis7_0): Unit = {
    import v.pubsub
    import v.spublish

    val names = OneOrMoreKeys.unsafeFrom(List(Key("pub:s"), Key("pub:none")))

    assertEquals(send(pubsub.shardchannels(GlobPattern("pub:*"))), Seq.empty[Key])
    assertEquals(send(spublish(Key("pub:s"), "m")), NonNegInt(0))

    subscribed("SSUBSCRIBE", "pub:s") {
      assert(send(pubsub.shardchannels).contains(Key("pub:s")))
      assertEquals(send(pubsub.shardchannels(GlobPattern("pub:*"))), Seq(Key("pub:s")))
      assertEquals(send(pubsub.shardnumsub(names)), Seq(Key("pub:s") -> NonNegInt(1), Key("pub:none") -> NonNegInt(0)))
      assertEquals(send(spublish(Key("pub:s"), "m")), NonNegInt(1))
    }
  }
}
