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

package laserdisc.fs2

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import cats.syntax.traverse.*
import laserdisc.*
import laserdisc.all.*
import laserdisc.auto.*
import laserdisc.fs2.SetUpLaserdiscCatsRespByte.LaserdiscState
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import org.apache.commons.pool2.impl.GenericObjectPoolConfig
import org.openjdk.jmh.annotations.*
import redis.clients.jedis.{Jedis, JedisPool}

object SetUpLaserdiscCatsRespByte {

  @State(Scope.Benchmark)
  class LaserdiscState {
    var runtime: IORuntime = null

    var client: RedisClient[IO]         = null
    private var clientCleanUp: IO[Unit] = null

    @Setup
    def setup(): Unit = {
      runtime = createNewRuntime()
      RedisClient[IO]
        .to("localhost", 6379)
        .allocated
        .map { case (rc, cu) =>
          client = rc
          clientCleanUp = cu
        }
        .unsafeRunSync()(using runtime)
    }

    @TearDown
    def tearDown(): Unit = {
      clientCleanUp.unsafeRunSync()(using runtime)
      runtime.shutdown()
    }
  }
}

@State(Scope.Benchmark)
class JedisState {
  var jedisPool: JedisPool = null

  var runtime: IORuntime = null

  @Setup
  def setup(): Unit = {
    val poolConfig = new GenericObjectPoolConfig
    poolConfig.setMaxTotal(8)
    poolConfig.setMinIdle(0)
    poolConfig.setMaxIdle(0)
    jedisPool = new JedisPool(poolConfig, "localhost", 6379)
    runtime = createNewRuntime()
  }

  @TearDown
  def tearDown(): Unit = {
    jedisPool.close()
    runtime.shutdown()
  }
}

class RedisClientBench {
  import RedisClientBench.*

  def writeAndReadForKey[K](key: K, n: Int, write: (K, Int) => IO[Any], read: K => IO[Any]): IO[Int] = {
    val values = List.fill(n)((key, n))
    values
      .traverse { case (key, value) =>
        write(key, value) *> read(key)
      }
      .map(_.size)
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def jedisWriteAccum100(jedisState: JedisState): Int =
    Resource
      .fromAutoCloseable[IO, Jedis](IO(jedisState.jedisPool.getResource))
      .use { jedisClient =>
        writeAndReadForKey[String](
          s"test-key:$numberOfItems",
          numberOfItems,
          (k, v) => IO(jedisClient.set(k, v.toString)),
          k => IO(jedisClient.get(k))
        )
      }
      .unsafeRunSync()(using jedisState.runtime)

  @Benchmark
  @OperationsPerInvocation(100)
  def laserdiscWriteAccum100(laserdiscState: LaserdiscState): Int = {
    val client = laserdiscState.client
    val key    = Key.unsafeFrom(s"test-key:$numberOfItems")
    writeAndReadForKey[Key](
      key,
      numberOfItems,
      (k, v) => client.send(set(k, v)),
      k => client.send(get(k))
    ).unsafeRunSync()(using laserdiscState.runtime)
  }
}

object RedisClientBench {
  val numberOfItems = 100
}
