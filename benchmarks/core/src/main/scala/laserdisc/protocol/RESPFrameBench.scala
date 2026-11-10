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
package protocol

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scodec.bits.BitVector

import laserdisc.RESPFrameFixture
import org.openjdk.jmh.infra.Blackhole

@State(Scope.Benchmark)
class RESPFrameBench {

  private object Fixture extends RESPFrameFixture
  import Fixture.*

  private final val mixedNoArr    = bytesOf(mixedNoArrList)
  private final val arrOneLevel   = bytesOf(arrOneLevelList)
  private final val arrFiveLevels = bytesOf(arrFiveLevelsList)

  private final val empty             = BitVector.empty
  private final val mixedNoArrFull    = BitVector(mixedNoArr)
  private final val arrOneLevelFull   = BitVector(arrOneLevel)
  private final val arrFiveLevelsFull = BitVector(arrFiveLevels)

  @Benchmark def frameOfFullBaseline(bh: Blackhole): Unit = {
    val frame = EmptyFrame.append(empty)
    bh.consume(frame)
  }
  @Benchmark def frameOfMixedNoArrFull(bh: Blackhole): Unit = {
    val frame = EmptyFrame.append(mixedNoArrFull)
    bh.consume(frame)
  }
  @Benchmark def frameOfMixedArrOneLevelFull(bh: Blackhole): Unit = {
    val frame = EmptyFrame.append(arrOneLevelFull)
    bh.consume(frame)
  }
  @Benchmark def frameOfMixedArrFiveLevelsFull(bh: Blackhole): Unit = {
    val frame = EmptyFrame.append(arrFiveLevelsFull)
    bh.consume(frame)
  }

  private final val mixedNoArrSmallChunkBuffers    = groupInChunks(mixedNoArr, 128)
  private final val arrOneLevelSmallChunkBuffers   = groupInChunks(arrOneLevel, 128)
  private final val arrFiveLevelsSmallChunkBuffers = groupInChunks(arrFiveLevels, 128)

  @Benchmark def frameOfChunkedBaseline(bh: Blackhole): Unit = {
    val frames = appendChunks(Iterator.empty)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedShortMixedNoArr(bh: Blackhole): Unit = {
    val frames = appendChunks(mixedNoArrSmallChunkBuffers)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedShortArrOneLevel(bh: Blackhole): Unit = {
    val frames = appendChunks(arrOneLevelSmallChunkBuffers)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedShortArrFiveLevels(bh: Blackhole): Unit = {
    val frames = appendChunks(arrFiveLevelsSmallChunkBuffers)
    bh.consume(frames)
  }

  private final val mixedNoArrBigChunkBuffers    = groupInChunks(mixedNoArr, 1024)
  private final val arrOneLevelBigChunkBuffers   = groupInChunks(arrOneLevel, 1024)
  private final val arrFiveLevelsBigChunkBuffers = groupInChunks(arrFiveLevels, 1024)

  @Benchmark def frameOfChunkedLongMixedNoArr(bh: Blackhole): Unit = {
    val frames = appendChunks(mixedNoArrBigChunkBuffers)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedLongArrOneLevel(bh: Blackhole): Unit = {
    val frames = appendChunks(arrOneLevelBigChunkBuffers)
    bh.consume(frames)
  }
  @Benchmark def frameOfChunkedLongArrFiveLevels(bh: Blackhole): Unit = {
    val frames = appendChunks(arrFiveLevelsBigChunkBuffers)
    bh.consume(frames)
  }
}
