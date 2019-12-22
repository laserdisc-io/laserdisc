package laserdisc
package protocol

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scodec.bits.BitVector
import eu.timepit.refined.types.string.NonEmptyString

import java.nio.ByteBuffer

@State(Scope.Benchmark)
class RESPFrameBench {

  val shortStr = " Short string repeated string repeated string repeated"
  val mediumStr = "Medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string"
  val longStr = "Very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very  very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very long string"

  val mixedNoArrList = List(
    StrEncoded(mediumStr),
    NullArrEncoded(),
    NumEncoded(21),
    StrEncoded(longStr),
    StrEncoded("OK"),
    BulkEncoded(NonEmptyString.unsafeFrom(longStr)),
    NumEncoded(150),
    StrEncoded(""),
    ErrEncoded(NonEmptyString.unsafeFrom(mediumStr)),
    BulkEncoded(NonEmptyString.unsafeFrom(shortStr)),
    NullBulkEncoded(),
    StrEncoded(longStr),
    StrEncoded("OK"),
    EmptyArrEncoded(),
    StrEncoded(longStr),
    BulkEncoded(NonEmptyString.unsafeFrom(mediumStr)),
    NumEncoded(200),
    NullBulkEncoded(),
    EmptyBulkEncoded(),
    StrEncoded(""),
    BulkEncoded(NonEmptyString.unsafeFrom(longStr)),
    NumEncoded(Long.MaxValue),
    StrEncoded(shortStr),
    NumEncoded(25),
    BulkEncoded(NonEmptyString.unsafeFrom("PONG")),
    EmptyArrEncoded(),
    StrEncoded(longStr),
    NullArrEncoded(),
    ErrEncoded(NonEmptyString.unsafeFrom(shortStr)),
    EmptyBulkEncoded(),
    StrEncoded("PONG"),
    NullArrEncoded()
  )

  val arrOneLevelList = List(
    NullArrEncoded(),
    NumEncoded(21),
    ArrEncoded(mixedNoArrList),
    StrEncoded("OK"),
    StrEncoded(""),
    NumEncoded(Long.MaxValue),
    StrEncoded(shortStr),
    StrEncoded("PONG")
  )

  val arrTwoLevelsList = List(
    NullArrEncoded(),
    NumEncoded(21),
    ArrEncoded(mixedNoArrList),
    StrEncoded("OK"),
    StrEncoded(""),
    ArrEncoded(arrOneLevelList),
    NumEncoded(Long.MaxValue),
    StrEncoded(shortStr),
    StrEncoded("PONG")
  )

  val arrThreeLevelsList = List(
    NullArrEncoded(),
    NumEncoded(21),
    ArrEncoded(mixedNoArrList),
    StrEncoded("OK"),
    StrEncoded(""),
    ArrEncoded(arrOneLevelList),
    NumEncoded(Long.MaxValue),
    StrEncoded(shortStr),
    ArrEncoded(arrTwoLevelsList),
    StrEncoded("PONG")
  )

  val arrFourLevelsList = List(
    NullArrEncoded(),
    ArrEncoded(arrThreeLevelsList),
    NumEncoded(21),
    ArrEncoded(mixedNoArrList),
    StrEncoded("OK"),
    StrEncoded(""),
    ArrEncoded(arrOneLevelList),
    NumEncoded(Long.MaxValue),
    StrEncoded(shortStr),
    ArrEncoded(arrTwoLevelsList),
    StrEncoded("PONG")
  )

  val arrFiveLevelsList = List(
    NullArrEncoded(),
    ArrEncoded(arrThreeLevelsList),
    NumEncoded(21),
    ArrEncoded(mixedNoArrList),
    StrEncoded("OK"),
    StrEncoded(""),
    ArrEncoded(arrOneLevelList),
    NumEncoded(Long.MaxValue),
    ArrEncoded(arrFourLevelsList),
    StrEncoded(shortStr),
    ArrEncoded(arrTwoLevelsList),
    StrEncoded("PONG")
  )

  val mixedNoArr = mixedNoArrList.map(_.encoded).mkString.getBytes
  val arrOneLevel = arrOneLevelList.map(_.encoded).mkString.getBytes
  val arrFiveLevels = arrFiveLevelsList.map(_.encoded).mkString.getBytes

  val mixedNoArrFull = BitVector(mixedNoArr).toByteBuffer
  val arrOneLevelFull = BitVector(arrOneLevel).toByteBuffer
  val arrFiveLevelsFull = BitVector(arrFiveLevels).toByteBuffer

  @Benchmark def frameOfMixedNoArrFull() = EmptyFrame.append(mixedNoArrFull)
  @Benchmark def frameOfMixedArrOneLevelFull() = EmptyFrame.append(arrOneLevelFull)
  @Benchmark def frameOfMixedArrFiveLevelsFull() = EmptyFrame.append(arrFiveLevelsFull)

  def groupInChunks(bytes :Array[Byte], chunkSize: Int) =
    bytes.grouped(chunkSize).map(c => BitVector.apply(c).toByteBuffer)

  def appendChunks(buff: Iterator[ByteBuffer]) =
    buff.foldLeft[RESPFrame](EmptyFrame) { (acc, n) =>
      acc.append(n) match {
        case Right(MoreThanOneFrame(_, reminder)) => IncompleteFrame(reminder, 0)
        case Right(fs)                            => fs
        case Left(e)                              => throw e
      }
    }

  val mixedNoArrSmallChunkBuffers    = groupInChunks(mixedNoArr, 100)
  val arrOneLevelSmallChunkBuffers   = groupInChunks(arrOneLevel, 100)
  val arrFiveLevelsSmallChunkBuffers = groupInChunks(arrFiveLevels, 100)

  @Benchmark def frameOfShortChunkedMixedNoArr()    = appendChunks(mixedNoArrSmallChunkBuffers)
  @Benchmark def frameOfShortChunkedArrOneLevel()   = appendChunks(arrOneLevelSmallChunkBuffers)
  @Benchmark def frameOfShortChunkedArrFiveLevels() = appendChunks(arrFiveLevelsSmallChunkBuffers)

  val mixedNoArrBigChunkBuffers    = groupInChunks(mixedNoArr, 1024)
  val arrOneLevelBigChunkBuffers   = groupInChunks(arrOneLevel, 1024)
  val arrFiveLevelsBigChunkBuffers = groupInChunks(arrFiveLevels, 1024)

  @Benchmark def frameOfLongChunkedMixedNoArr()    = appendChunks(mixedNoArrBigChunkBuffers)
  @Benchmark def frameOfLongChunkedArrOneLevel()   = appendChunks(arrOneLevelBigChunkBuffers)
  @Benchmark def frameOfLongChunkedArrFiveLevels() = appendChunks(arrFiveLevelsBigChunkBuffers)
}
