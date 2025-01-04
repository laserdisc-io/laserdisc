/*
 * Copyright (c) 2018-2025 LaserDisc
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

import eu.timepit.refined.types.string.NonEmptyString
import laserdisc.protocol._
import org.scalacheck.Gen.chooseNum
import org.scalacheck.{Arbitrary, Gen}
import scodec.bits.BitVector

import scala.Long.{MaxValue, MinValue}

private[laserdisc] trait RESPFrameFixture extends HighPriorityGenerators {
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
      accFrame.append(n) match {
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

private[laserdisc] trait HighPriorityGenerators extends LowPriorityGenerators {

  private[this] def oneOrMoreProtocol(gen: Gen[ProtocolEncoded]): Gen[OneOrMore[ProtocolEncoded]] =
    Gen.chooseNum(1, Math.min(scalaCheckTestParameters.maxSize, 300)) flatMap (Gen.listOfN(_, gen)) map OneOrMore.unsafeFrom

  private[this] def listProtocol(gen: Gen[ProtocolEncoded]): Gen[List[ProtocolEncoded]] =
    Gen.chooseNum(1, 20) flatMap (Gen.listOfN(_, gen))

  private[this] final def noArrEncoded(
      implicit bulk: Gen[BulkEncoded],
      num: Gen[NumEncoded],
      str: Gen[StrEncoded],
      err: Gen[ErrEncoded],
      emptyBulk: Gen[EmptyBulkEncoded],
      nullBulk: Gen[NullBulkEncoded],
      emptyArr: Gen[EmptyArrEncoded],
      nullArr: Gen[NullArrEncoded]
  ): Gen[ProtocolEncoded] =
    Gen.frequency(
      10 -> bulk,
      10 -> num,
      10 -> str,
      10 -> err,
      3  -> emptyBulk,
      3  -> nullBulk,
      3  -> emptyArr,
      3  -> nullArr
    )

  private[this] final val noArrArrEncoded: Gen[ArrEncoded] =
    listProtocol(noArrEncoded) map ArrEncoded.apply

  private[this] final def oneLevelArrEncoded(arrGen: Gen[ArrEncoded]): Gen[ArrEncoded] =
    listProtocol(
      Gen.frequency(
        20 -> noArrEncoded,
        3  -> arrGen
      )
    ) map ArrEncoded.apply

  private[this] final def xLevelsNestedArrEncoded(x: Int): Gen[ArrEncoded] = {
    @scala.annotation.tailrec
    def loop(left: Int, soFar: Gen[ArrEncoded]): Gen[ArrEncoded] =
      if (left > 0) loop(left - 1, oneLevelArrEncoded(soFar))
      else soFar

    loop(x, oneLevelArrEncoded(noArrArrEncoded))
  }

  private[this] final val protocolEncoded: Gen[ProtocolEncoded] =
    Gen.frequency(
      50 -> noArrEncoded,
      25 -> oneLevelArrEncoded(noArrArrEncoded),
      25 -> noArrArrEncoded,
      10 -> xLevelsNestedArrEncoded(5)
    )

  private[laserdisc] implicit final val oneOrMoreProtocols: Arbitrary[OneOrMore[ProtocolEncoded]] =
    Arbitrary(oneOrMoreProtocol(protocolEncoded))
}

private[laserdisc] trait LowPriorityGenerators extends BaseSpec {
  protected implicit final def string: Gen[String] = Gen.listOf(utf8BMPCharGen) map (_.mkString)

  protected implicit final def nonEmptyString(implicit strGen: Gen[String]): Gen[NonEmptyString] =
    strGen.filter(_.nonEmpty) map NonEmptyString.unsafeFrom

  protected implicit final def bulkEncoded(implicit nesGen: Gen[NonEmptyString]): Gen[BulkEncoded] =
    nesGen map BulkEncoded.apply

  protected implicit final val long: Gen[Long] = chooseNum(MinValue, MaxValue)

  protected implicit final def numEncoded(implicit lnGen: Gen[Long]): Gen[NumEncoded] =
    lnGen map NumEncoded.apply

  protected implicit final def strEncoded(implicit sGen: Gen[String]): Gen[StrEncoded] =
    sGen.map(s => s.replace(CRLF, "")).filter(_.nonEmpty) map StrEncoded.apply

  protected implicit final def errEncoded(implicit nesGen: Gen[NonEmptyString]): Gen[ErrEncoded] =
    nesGen map ErrEncoded.apply

  protected implicit final val emptyBulkEncoded: Gen[EmptyBulkEncoded] =
    Gen.const(EmptyBulkEncoded())

  protected implicit final val nullBulkEncoded: Gen[NullBulkEncoded] =
    Gen.const(NullBulkEncoded())

  protected implicit final val emptyArrEncoded: Gen[EmptyArrEncoded] =
    Gen.const(EmptyArrEncoded())

  protected implicit final val nullArrEncoded: Gen[NullArrEncoded] =
    Gen.const(NullArrEncoded())
}
