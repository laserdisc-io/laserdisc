package laserdisc.fs2

import cats.effect._
import cats.implicits._
import laserdisc._
import laserdisc.all._
import laserdisc.auto._
import log.effect.LogWriter
import log.effect.fs2.SyncLogWriter
import redis.clients.jedis.Jedis
import org.openjdk.jmh.annotations._

import scala.concurrent.ExecutionContext

object shared {
  implicit def contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
  implicit val timer: Timer[IO]               = IO.timer(ExecutionContext.global)

  implicit val logWriter: LogWriter[IO] = SyncLogWriter.noOpLog

  @State(Scope.Benchmark)
  val redisClientResource: Resource[IO, RedisClient[IO]] = RedisClient.toNode[IO]("localhost", 6379)

  @State(Scope.Benchmark)
  val jedisClient: Jedis = new Jedis("localhost", 6379)
}

class RedisClientBench {
  import shared._

  def writeAndReadForKey[K](key: K, n: Int, write: (K, Int) => IO[_], read: K => IO[_]): IO[Int] = {
    val values = List.fill(n)((key, n))
    values
      .traverse {
        case (key, value) => write(key, value) *> read(key)
      }
      .map(_.size)
  }

  def writeAndReadLaserdisc(n: Int): Int = {
    redisClientResource
      .use { redisClient =>
        val key = Key.unsafeFrom(s"test-key:$n")
        writeAndReadForKey[Key](
          key,
          n,
          (k, v) => redisClient.send(set(k, v)),
          k => redisClient.send(get(k))
        )
      }
      .unsafeRunSync()
  }

  def writeAndReadJedis(n: Int): Int = {
    writeAndReadForKey[String](
      s"test-key:$n",
      n,
      (k, v) => IO(jedisClient.set(k, v.toString)),
      k => IO(jedisClient.get(k))
    ).unsafeRunSync()
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def jedis_write_accum_100: Int = writeAndReadJedis(100)

  @Benchmark
  @OperationsPerInvocation(100)
  def laserdisc_write_accum_100: Int = writeAndReadLaserdisc(100)
}
