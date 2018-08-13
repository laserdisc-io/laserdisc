package laserdisc.protocol

import eu.timepit.refined.types.string.NonEmptyString
import laserdisc.OneOrMore
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpecLike}
import scodec.bits.BitVector

final class RESPFrameMixedSpec extends WordSpecLike with Matchers with PropertyChecks {

  "A non empty mixed Frame" when {

    "appending a bit vector composed of a complete sequence of integers, simple strings, bulk strings and errors" should {
      "produce MoreThanOne with a list of all the complete items" in {
        val nonEmptyFrame = Incomplete(BitVector("$16\r\nTest bulk str".getBytes), 0)
        val inputVector = BitVector("ing\r\n+OK\r\n+Another simple string\r\n-Possible error message\r\n:1\r\n:2\r\n:177\r\n+Another simple string\r\n$21\r\nTest bulk string 1 11\r\n-And an error message\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer).fold(
          err => fail(s"expected a result but failed with $err"),
          {
            case r@MoreThanOne(_, _) => r.complete shouldBe Vector(
              Complete(BitVector("$16\r\nTest bulk string\r\n".getBytes())),
              Complete(BitVector("+OK\r\n".getBytes())),
              Complete(BitVector("+Another simple string\r\n".getBytes())),
              Complete(BitVector("-Possible error message\r\n".getBytes())),
              Complete(BitVector(":1\r\n".getBytes())),
              Complete(BitVector(":2\r\n".getBytes())),
              Complete(BitVector(":177\r\n".getBytes())),
              Complete(BitVector("+Another simple string\r\n".getBytes())),
              Complete(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
              Complete(BitVector("-And an error message\r\n".getBytes()))
            )
            case _ => fail(s"expected a MoreThanOne type")
          }
        )
      }
    }

    "appending a bit vector composed of sequence of integers, simple strings, bulk strings and errors that are not complete" should {
      "produce MoreThanOne with a list of all the complete items plus the remainder" in {
        val nonEmptyFrame = Incomplete(BitVector("$16\r\nTest bulk str".getBytes), 0)
        val inputVector = BitVector("ing\r\n+OK\r\n+Another simple string\r\n-Possible error message\r\n:1\r\n:2\r\n:177\r\n+Another simple string\r\n$21\r\nTest bulk string 1 11\r\n-And an error message\r\n".getBytes)
        nonEmptyFrame.append(inputVector.toByteBuffer).fold(
          err => fail(s"expected a result but failed with $err"),
          {
            case r@MoreThanOne(_, _) => r.complete shouldBe Vector(
              Complete(BitVector("$16\r\nTest bulk string\r\n".getBytes())),
              Complete(BitVector("+OK\r\n".getBytes())),
              Complete(BitVector("+Another simple string\r\n".getBytes())),
              Complete(BitVector("-Possible error message\r\n".getBytes())),
              Complete(BitVector(":1\r\n".getBytes())),
              Complete(BitVector(":2\r\n".getBytes())),
              Complete(BitVector(":177\r\n".getBytes())),
              Complete(BitVector("+Another simple string\r\n".getBytes())),
              Complete(BitVector("$21\r\nTest bulk string 1 11\r\n".getBytes())),
              Complete(BitVector("-And an error message\r\n".getBytes()))
            )
            case _ => fail(s"expected a MoreThanOne type")
          }
        )
      }
    }

  }

  "An empty Frame" when {
    "appending a random sequence of complete messages" should {
      "produce MoreThanOne with all the complete items" in {
        forAll { content: OneOrMore[String] =>
          EmptyFrame.append(BitVector(content.value.mkString.getBytes()).toByteBuffer).fold(
            err => fail(s"expected a result but failed with $err"),
            {
              case r@MoreThanOne(_, _) =>
                r.complete shouldNot be (empty)
                r.remainder should be (empty)
              case _ => fail(s"expected a MoreThanOne type")
            }
          )
        }
      }
    }
  }

  private implicit def arbitraryNonEmptyString(implicit ev: Arbitrary[String]): Arbitrary[NonEmptyString] =
    Arbitrary {
      ev.arbitrary.filter(_.length > 0) map NonEmptyString.unsafeFrom
    }

  private implicit def arbitraryMessages(implicit ev1: Arbitrary[Int], ev2: Arbitrary[NonEmptyString]): Arbitrary[OneOrMore[String]] =
    Arbitrary {
      (for {
        n  <- Gen.choose(2, 1000)
        i  <- ev1.arbitrary map (n => s":$n\r\n")
        s  <- ev2.arbitrary map (st => s"+$st\r\n")
        e  <- ev2.arbitrary map (st => s"-$st\r\n")
        bs <- ev2.arbitrary map (st => s"$$${st.value.length}\r\n$st\r\n")
        xs <- Gen.listOfN(n, Gen.oneOf(
                Seq(
                  //i, s, e, bs,
                  "$16\r\nTest bulk string\r\n",
                  "+OK\r\n",
                  "+a\r\n",
                  "+Another simple string\r\n",
                  "-Possible error message\r\n",
                  "-1234 Error with numbers\r\n",
                  ":1\r\n",
                  ":2\r\n",
                  ":177\r\n",
                  ":123456789\r\n",
                  ":-1\r\n",
                  "+Very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very very long single line string\r\n",
                  "+Another simple string\r\n",
                  "$21\r\nTest bulk string 1 11\r\n",
                  "$-1\r\n",
                  "-And an error message\r\n",
                  "$21\r\nTest bulk string 1 11\r\n$17\r\nTest bulk string2\r\n$20\r\nTest bulk string 3 1\r\n$19\r\nTest bulk string 40\r\n"
                )
              ))
      } yield xs) map (xs => OneOrMore.unsafeFrom(xs))
    }
}
