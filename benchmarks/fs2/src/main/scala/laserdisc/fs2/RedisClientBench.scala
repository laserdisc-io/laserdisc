package laserdisc.fs2

import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import cats.syntax.traverse._
import laserdisc._
import laserdisc.all._
import laserdisc.auto._
import laserdisc.fs2.SetUpLaserdiscCatsRespByte.LaserdiscState
import laserdisc.fs2.parallel.runtime.BenchRuntime.createNewRuntime
import org.apache.commons.pool2.impl.GenericObjectPoolConfig
import org.openjdk.jmh.annotations._
import redis.clients.jedis.{Jedis, JedisPool}

object SetUpLaserdiscCatsRespByte {

  @State(Scope.Benchmark)
  class LaserdiscState {
    var runtime: IORuntime = _

    var client: RedisClient[IO]         = _
    private var clientCleanUp: IO[Unit] = _

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
        .unsafeRunSync()(runtime)
    }

    @TearDown
    def tearDown(): Unit = {
      clientCleanUp.unsafeRunSync()(runtime)
      runtime.shutdown()
    }
  }
}

@State(Scope.Benchmark)
class JedisState {
  var jedisPool: JedisPool = _

  var runtime: IORuntime = _

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
  import RedisClientBench._

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
      .unsafeRunSync()(jedisState.runtime)

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
    ).unsafeRunSync()(laserdiscState.runtime)
  }
}

object RedisClientBench {
  val numberOfItems = 100
}
