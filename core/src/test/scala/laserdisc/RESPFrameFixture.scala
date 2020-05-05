package laserdisc

import eu.timepit.refined.types.string.NonEmptyString
import laserdisc.protocol._
import scodec.bits.BitVector

private[laserdisc] trait RESPFrameFixture {
  final val shortStr = " Short string repeated string repeated string repeated"
  final val mediumStr =
    "Medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string repeated medium size string"
  final val longStr =
    "Very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very  very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very long string"

  final val mixedNoArrList = List(
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

  final val arrOneLevelList = List(
    NullArrEncoded(),
    NumEncoded(21),
    ArrEncoded(mixedNoArrList),
    StrEncoded("OK"),
    StrEncoded(""),
    NumEncoded(Long.MaxValue),
    StrEncoded(shortStr),
    StrEncoded("PONG")
  )

  final val arrTwoLevelsList = List(
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

  final val arrThreeLevelsList = List(
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

  final val arrFourLevelsList = List(
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

  final val arrFiveLevelsList = List(
    NullArrEncoded(),
    StrEncoded("OK"),
    ArrEncoded(arrThreeLevelsList),
    NumEncoded(21),
    ArrEncoded(mixedNoArrList),
    StrEncoded(""),
    ArrEncoded(arrOneLevelList),
    NumEncoded(Long.MaxValue),
    ArrEncoded(arrFourLevelsList),
    StrEncoded(shortStr),
    ArrEncoded(arrTwoLevelsList),
    StrEncoded("PONG")
  )

  final def bytesOf(l: List[ProtocolEncoded]): Array[Byte] =
    l.map(_.encoded).mkString.getBytes

  final def groupInChunks(bytes: Array[Byte], chunkSize: Int): Iterator[BitVector] =
    bytes.grouped(chunkSize).map(c => BitVector.apply(c))

  final def appendChunks(buff: Iterator[BitVector]): Vector[CompleteFrame] = {
    val aggregate = buff.foldLeft[(RESPFrame, Vector[CompleteFrame])](
      EmptyFrame -> Vector.empty[CompleteFrame]
    ) { (acc, n) =>
      val (accFrame, completed) = acc
      accFrame.append(n.toByteBuffer) match {
        case Right(CompleteFrame(c)) =>
          EmptyFrame -> (completed :+ CompleteFrame(c))
        case Right(MoreThanOneFrame(c, reminder)) =>
          if (reminder.size > 0)
            IncompleteFrame(reminder, 0) -> (completed ++ c)
          else
            EmptyFrame -> (completed ++ c)
        case Right(IncompleteFrame(p, _)) =>
          IncompleteFrame(p, 0) -> completed
        case Left(e) => throw e
      }
    }
    aggregate._2
  }
}
