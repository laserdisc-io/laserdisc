package laserdisc
package protocol

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scodec.bits.BitVector
import eu.timepit.refined.types.string.NonEmptyString
import java.nio.ByteBuffer

import laserdisc.RESPFrameFixture
import org.openjdk.jmh.infra.Blackhole

@State(Scope.Benchmark)
class RESPFrameBench {

  private[this] object Fixture extends RESPFrameFixture
  import Fixture._

  val mixedNoArr = bytesOf(mixedNoArrList)
  val arrOneLevel = bytesOf(arrOneLevelList)
  val arrFiveLevels = bytesOf(arrFiveLevelsList)

  val empty = BitVector.empty
  val mixedNoArrFull = BitVector(mixedNoArr)
  val arrOneLevelFull = BitVector(arrOneLevel)
  val arrFiveLevelsFull = BitVector(arrFiveLevels)

  @Benchmark def frameOfFullBaseline(bh: Blackhole)= {
    val frame = EmptyFrame.append(empty)
    bh.consume(frame)
  }
  @Benchmark def frameOfMixedNoArrFull(bh: Blackhole) = {
    val frame = EmptyFrame.append(mixedNoArrFull)
    bh.consume(frame)
  }
  @Benchmark def frameOfMixedArrOneLevelFull(bh: Blackhole) = {
    val frame = EmptyFrame.append(arrOneLevelFull)
    bh.consume(frame)
  }
  @Benchmark def frameOfMixedArrFiveLevelsFull(bh: Blackhole) = {
    val frame = EmptyFrame.append(arrFiveLevelsFull)
    bh.consume(frame)
  }

  val mixedNoArrSmallChunkBuffers    = groupInChunks(mixedNoArr, 128)
  val arrOneLevelSmallChunkBuffers   = groupInChunks(arrOneLevel, 128)
  val arrFiveLevelsSmallChunkBuffers = groupInChunks(arrFiveLevels, 128)

  @Benchmark def frameOfChunkedBaseline(bh: Blackhole)= {
    val frames = appendChunks(Iterator.empty[BitVector])
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedShortMixedNoArr(bh: Blackhole)= {
    val frames = appendChunks(mixedNoArrSmallChunkBuffers)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedShortArrOneLevel(bh: Blackhole)   = {
    val frames = appendChunks(arrOneLevelSmallChunkBuffers)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedShortArrFiveLevels(bh: Blackhole) = {
    val frames = appendChunks(arrFiveLevelsSmallChunkBuffers)
    bh.consume(frames)
  }

  val mixedNoArrBigChunkBuffers    = groupInChunks(mixedNoArr, 1024)
  val arrOneLevelBigChunkBuffers   = groupInChunks(arrOneLevel, 1024)
  val arrFiveLevelsBigChunkBuffers = groupInChunks(arrFiveLevels, 1024)

  @Benchmark def frameOfChunkedLongMixedNoArr(bh: Blackhole)    = {
    val frames = appendChunks(mixedNoArrBigChunkBuffers)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedLongArrOneLevel(bh: Blackhole)   = {
    val frames = appendChunks(arrOneLevelBigChunkBuffers)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedLongArrFiveLevels(bh: Blackhole) = {
    val frames = appendChunks(arrFiveLevelsBigChunkBuffers)
    bh.consume(frames)
  }
}
