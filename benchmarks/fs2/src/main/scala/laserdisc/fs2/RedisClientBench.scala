package laserdisc.fs2

import java.util.concurrent.{ExecutorService, Executors}

import cats.effect.{ContextShift, IO, Resource, Timer}
import cats.implicits._
import laserdisc._
import laserdisc.all._
import laserdisc.auto._
import log.effect.LogWriter
import log.effect.fs2.SyncLogWriter
import org.apache.commons.pool2.impl.GenericObjectPoolConfig
import redis.clients.jedis.{Jedis, JedisPool}
import org.openjdk.jmh.annotations._

import scala.concurrent.ExecutionContext

trait Pooling {
  val poolSize: Int = 8
}

@State(Scope.Benchmark)
class Base extends Pooling {
  private var executorService: ExecutorService = _
  implicit var contextShift: ContextShift[IO]  = _
  implicit var timer: Timer[IO]                = _

  @Setup
  def setupBase(): Unit = {
    executorService = Executors.newFixedThreadPool(poolSize)
    contextShift = IO.contextShift(ExecutionContext.fromExecutor(executorService))
    timer = IO.timer(ExecutionContext.global)
  }

  @TearDown
  def tearDownBase(): Unit = {
    contextShift = null
    timer = null
    executorService.shutdown()
  }
}

@State(Scope.Benchmark)
class LaserdiscState {
  var client: RedisClient[IO]                 = _
  private var clientShutdownProcess: IO[Unit] = _
  var base: Base                              = _

  @Setup
  def setup(base: Base): Unit = {
    import base._
    this.base = base

    val clientResource = RedisClient.to("localhost", 6379).allocated.unsafeRunSync()
    client = clientResource._1
    clientShutdownProcess = clientResource._2
  }

  @TearDown
  def tearDown(): Unit =
    clientShutdownProcess.unsafeRunSync()
}

@State(Scope.Benchmark)
class JedisState extends Pooling {
  var jedisPool: JedisPool = _

  @Setup
  def setup(): Unit = {
    val poolConfig = new GenericObjectPoolConfig
    poolConfig.setMaxTotal(poolSize)
    poolConfig.setMinIdle(0)
    poolConfig.setMaxIdle(0)
    jedisPool = new JedisPool(poolConfig, "localhost", 6379)
  }

  @TearDown
  def tearDown(): Unit =
    jedisPool.close()
}

class RedisClientBench {
  import RedisClientBench._

  def writeAndReadForKey[K](key: K, n: Int, write: (K, Int) => IO[Any], read: K => IO[Any]): IO[Int] = {
    val values = List.fill(n)((key, n))
    values
      .traverse {
        case (key, value) => write(key, value) *> read(key)
      }
      .map(_.size)
  }

  def writeAndReadLaserdisc(laserdiscClient: RedisClient[IO], n: Int)(implicit contextShift: ContextShift[IO], timer: Timer[IO]): Int = {
    val key = Key.unsafeFrom(s"test-key:$n")
    writeAndReadForKey[Key](
      key,
      n,
      (k, v) => laserdiscClient.send(set(k, v)),
      k => laserdiscClient.send(get(k))
    ).unsafeRunSync()
  }

  def writeAndReadJedis(jedisPool: JedisPool, n: Int): Int =
    Resource
      .fromAutoCloseable[IO, Jedis](IO(jedisPool.getResource))
      .use { jedisClient =>
        writeAndReadForKey[String](
          s"test-key:$n",
          n,
          (k, v) => IO(jedisClient.set(k, v.toString)),
          k => IO(jedisClient.get(k))
        )
      }
      .unsafeRunSync()

  @Benchmark
  @OperationsPerInvocation(20)
  def jedis_write_accum_100(jedisState: JedisState): Int = writeAndReadJedis(jedisState.jedisPool, numberOfItems)

  @Benchmark
  @OperationsPerInvocation(20)
  def laserdisc_write_accum_100(laserdiscState: LaserdiscState): Int = {
    val base = laserdiscState.base
    import base._
    writeAndReadLaserdisc(laserdiscState.client, numberOfItems)
  }
}

object RedisClientBench {
  val numberOfItems = 50
}
