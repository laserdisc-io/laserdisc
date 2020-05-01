package laserdisc
package protocol

import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Gen.chooseNum
import org.scalacheck.{Arbitrary, Gen}
import scodec.bits.BitVector

import scala.Long.{MaxValue, MinValue}

final class RESPFrameMixedSpec extends HighPriorityGenerators {
  "A non empty mixed Frame" when {
    "appending a bit vector composed of a complete sequence of integers, simple strings, bulk strings and errors" should {
      "produce MoreThanOne with a list of all the complete items" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk str".getBytes), 0)
        val inputVector = BitVector(
          "ing\r\n+OK\r\n$0\r\n\r\n+Another simple string\r\n*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n-Possible error message\r\n*0\r\n:1\r\n:2\r\n*2\r\n$8\r\nAnother1\r\n-An error\r\n:177\r\n+Another simple string\r\n$21\r\nTest bulk string 1 11\r\n*5\r\n$16\r\nTest bulk string\r\n:13\r\n-1234 An error with numbers\r\n:100\r\n+A simple string\r\n-And an error message\r\n".getBytes
        )
        nonEmptyFrame.append(inputVector.toByteBuffer) onRight {
          case r @ MoreThanOneFrame(_, _) =>
            r.complete shouldBe Vector(
              CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes())),
              CompleteFrame(BitVector("+OK\r\n".getBytes())),
              CompleteFrame(BitVector("$0\r\n\r\n".getBytes())),
              CompleteFrame(BitVector("+Another simple string\r\n".getBytes())),
              CompleteFrame(BitVector("*3\r\n$16\r\nTest bulk string\r\n:100\r\n+A simple string\r\n".getBytes)),
              CompleteFrame(BitVector("-Possible error message\r\n".getBytes())),
              CompleteFrame(BitVector("*0\r\n".getBytes())),
              CompleteFrame(BitVector(":1\r\n".getBytes())),
              CompleteFrame(BitVector(":2\r\n".getBytes())),
              CompleteFrame(BitVector("*2\r\n$8\r\nAnother1\r\n-An error\r\n".getBytes)),
              CompleteFrame(BitVector(":177\r\n".getBytes())),
              CompleteFrame(BitVector("+Another simple string\r\n".getBytes())),
              CompleteFrame(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
              CompleteFrame(
                BitVector(
                  "*5\r\n$16\r\nTest bulk string\r\n:13\r\n-1234 An error with numbers\r\n:100\r\n+A simple string\r\n".getBytes
                )
              ),
              CompleteFrame(BitVector("-And an error message\r\n".getBytes()))
            )
          case _ => fail(s"expected a MoreThanOne type")
        }
      }
    }

    "appending a bit vector composed of sequence of integers, simple strings, bulk strings and errors that are not complete" should {
      "produce MoreThanOne with a list of all the complete items plus the remainder" in {
        val nonEmptyFrame = IncompleteFrame(BitVector("$16\r\nTest bulk str".getBytes), 0)
        val inputVector = BitVector(
          "ing\r\n+OK\r\n+Another simple string\r\n-Possible error message\r\n:1\r\n:2\r\n:177\r\n+Another simple string\r\n$21\r\nTest bulk string 1 11\r\n-And an error message\r\n".getBytes
        )
        nonEmptyFrame.append(inputVector.toByteBuffer) onRight {
          case r @ MoreThanOneFrame(_, _) =>
            r.complete shouldBe Vector(
              CompleteFrame(BitVector("$16\r\nTest bulk string\r\n".getBytes())),
              CompleteFrame(BitVector("+OK\r\n".getBytes())),
              CompleteFrame(BitVector("+Another simple string\r\n".getBytes())),
              CompleteFrame(BitVector("-Possible error message\r\n".getBytes())),
              CompleteFrame(BitVector(":1\r\n".getBytes())),
              CompleteFrame(BitVector(":2\r\n".getBytes())),
              CompleteFrame(BitVector(":177\r\n".getBytes())),
              CompleteFrame(BitVector("+Another simple string\r\n".getBytes())),
              CompleteFrame(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
              CompleteFrame(BitVector("-And an error message\r\n".getBytes()))
            )
          case _ => fail(s"expected a MoreThanOne type")
        }
      }
    }
  }

  "An empty Frame" when {
    "appending a random sequence of complete messages" should {
      "produce MoreThanOne with all the complete items" in {
        forAll { testSet: OneOrMore[ProtocolEncoded] =>
          val vector = BitVector(testSet.value.map(_.encoded).mkString.getBytes)

          EmptyFrame.append(vector.toByteBuffer) onRight {
            case MoreThanOneFrame(complete, remainder) =>
              complete.size shouldBe testSet.value.size
              remainder should be(empty)
            case CompleteFrame(_) => succeed
            case other            => fail(s"expected a MoreThanOne type. Was $other")
          }
        }
      }
    }
  }
}

private[protocol] trait HighPriorityGenerators extends LowPriorityGenerators {

  private[this] def oneOrMoreProtocol(gen: Gen[ProtocolEncoded]): Gen[OneOrMore[ProtocolEncoded]] =
    Gen.chooseNum(1, Math.min(generatorDrivenConfig.sizeRange, 300)) flatMap (Gen.listOfN(_, gen)) map OneOrMore.unsafeFrom

  private[this] def listProtocol(gen: Gen[ProtocolEncoded]): Gen[List[ProtocolEncoded]] =
    Gen.chooseNum(1, 20) flatMap (Gen.listOfN(_, gen))

  final private[this] def noArrEncoded(
      implicit
      bulk: Gen[BulkEncoded],
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

  final private[this] val noArrArrEncoded: Gen[ArrEncoded] =
    listProtocol(noArrEncoded) map ArrEncoded.apply

  final private[this] def oneLevelArrEncoded(arrGen: Gen[ArrEncoded]): Gen[ArrEncoded] =
    listProtocol(
      Gen.frequency(
        20 -> noArrEncoded,
        3  -> arrGen
      )
    ) map ArrEncoded.apply

  final private[this] def xLevelsNestedArrEncoded(x: Int): Gen[ArrEncoded] = {

    @scala.annotation.tailrec
    def loop(left: Int, soFar: Gen[ArrEncoded]): Gen[ArrEncoded] =
      if (left > 0) loop(left - 1, oneLevelArrEncoded(soFar))
      else soFar

    loop(x, oneLevelArrEncoded(noArrArrEncoded))
  }

  final private[this] val protocolEncoded: Gen[ProtocolEncoded] =
    Gen.frequency(
      50 -> noArrEncoded,
      25 -> oneLevelArrEncoded(noArrArrEncoded),
      25 -> noArrArrEncoded,
      10 -> xLevelsNestedArrEncoded(5)
    )

  implicit final private[protocol] val oneOrMoreProtocols: Arbitrary[OneOrMore[ProtocolEncoded]] =
    Arbitrary(oneOrMoreProtocol(protocolEncoded))
}

private[protocol] trait LowPriorityGenerators extends BaseSpec {
  implicit final protected def string: Gen[String] = Gen.listOf(utf8BMPCharGen) map (_.mkString)

  implicit final protected def nonEmptyString(implicit strGen: Gen[String]): Gen[NonEmptyString] =
    strGen.filter(_.nonEmpty) map NonEmptyString.unsafeFrom

  implicit final protected def bulkEncoded(implicit nesGen: Gen[NonEmptyString]): Gen[BulkEncoded] =
    nesGen map BulkEncoded.apply

  implicit final protected val long: Gen[Long] = chooseNum(MinValue, MaxValue)

  implicit final protected def numEncoded(implicit lnGen: Gen[Long]): Gen[NumEncoded] =
    lnGen map NumEncoded.apply

  implicit final protected def strEncoded(implicit sGen: Gen[String]): Gen[StrEncoded] =
    sGen.map(s => s.replace(CRLF, "")).filter(_.nonEmpty) map StrEncoded.apply

  implicit final protected def errEncoded(implicit nesGen: Gen[NonEmptyString]): Gen[ErrEncoded] =
    nesGen map ErrEncoded.apply

  implicit final protected val emptyBulkEncoded: Gen[EmptyBulkEncoded] =
    Gen.const(EmptyBulkEncoded())

  implicit final protected val nullBulkEncoded: Gen[NullBulkEncoded] =
    Gen.const(NullBulkEncoded())

  implicit final protected val emptyArrEncoded: Gen[EmptyArrEncoded] =
    Gen.const(EmptyArrEncoded())

  implicit final protected val nullArrEncoded: Gen[NullArrEncoded] =
    Gen.const(NullArrEncoded())
}
