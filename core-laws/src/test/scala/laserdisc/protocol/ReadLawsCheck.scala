package laserdisc
package protocol

import cats.instances.int._
import cats.instances.long._
import cats.instances.string._
import cats.laws.discipline.{ContravariantTests, MonadTests, SerializableTests}
import cats.{Contravariant, Eq, Monad}
import org.scalacheck.Gen.chooseNum
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.FunSuiteDiscipline

final class ReadLawsCheck
    extends AnyFunSuiteLike
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with FunSuiteDiscipline
    with Configuration
    with Implicits {

  import ReadInstances._

  checkAll("Read[Num, *]", MonadTests[Read[Num, *]].stackUnsafeMonad[Long, String, Long])
  checkAll("Monad[Read[Num, *]]", SerializableTests.serializable(Monad[Read[Num, *]]))

  checkAll("Read[*, Long]", ContravariantTests[Read[*, Long]].contravariant[Str, Num, Str])
  checkAll("Contravariant[Read[*, Long]]", SerializableTests.serializable(Contravariant[Read[*, Long]]))
}

private[protocol] sealed trait Implicits {
  implicit val genNum: Gen[Num] = chooseNum(0L, Long.MaxValue) map Num.apply
  implicit val genStr: Gen[Str] = Gen.alphaNumStr map Str.apply

  implicit val genListOfNum: Gen[List[Num]] = Gen.listOfN(500, genNum) map (_.distinct)
  implicit val genListOfStr: Gen[List[Str]] = Gen.listOfN(500, genStr) map (_.distinct)

  implicit def arbNum(implicit ev: Gen[Num]): Arbitrary[Num] = Arbitrary(ev)
  implicit def arbStr(implicit ev: Gen[Str]): Arbitrary[Str] = Arbitrary(ev)

  implicit val cogenNum: Cogen[Num] = Cogen(_.value)
  implicit val cogenStr: Cogen[Str] = Cogen(_.value.length.toLong)

  implicit def arbReadNum(implicit ev: Arbitrary[Long]): Arbitrary[Read[Num, Long]] = Arbitrary(ev.arbitrary map Read.const)
  implicit def arbReadStr(implicit ev: Arbitrary[Long]): Arbitrary[Read[Str, Long]] = Arbitrary(ev.arbitrary map Read.const)

  implicit def arbReadNumString(implicit ev: Arbitrary[Num]): Arbitrary[Read[Num, String]] =
    Arbitrary(ev.arbitrary map (n => Read.const(n.value.toString)))

  implicit def arbReadNumStringFun(implicit ev: Arbitrary[Num]): Arbitrary[Read[Num, Long => String]] =
    Arbitrary(ev.arbitrary map (l => Read.const(_ => l.value.toString)))

  implicit def arbReadNumNumFun(implicit ev: Arbitrary[String]): Arbitrary[Read[Num, String => Long]] =
    Arbitrary(ev.arbitrary map (s => Read.const(_ => s.length.toLong)))

  implicit def eqReadTup[A, B, C, D](implicit ga: Gen[List[A]], eb: Eq[B], ec: Eq[C], ed: Eq[D]): Eq[Read[A, (B, C, D)]] =
    new Eq[Read[A, (B, C, D)]] {
      override def eqv(x: ==>[A, (B, C, D)], y: ==>[A, (B, C, D)]): Boolean = {
        val as = ga.sample.get
        as.forall { a =>
          (x.read(a), y.read(a)) match {
            case (Right((b1, c1, d1)), Right((b2, c2, d2))) => eb.eqv(b1, b2) && ec.eqv(c1, c2) && ed.eqv(d1, d2)
            case (Left(e1), Left(e2))                       => e1.message == e2.message
            case _                                          => false
          }
        }
      }
    }

  implicit def eqRead[A, B](implicit ga: Gen[List[A]], eb: Eq[B]): Eq[Read[A, B]] =
    new Eq[Read[A, B]] {
      override def eqv(x: A ==> B, y: A ==> B): Boolean = {
        val as = ga.sample.get
        as.forall { a =>
          (x.read(a), y.read(a)) match {
            case (Right(b1), Right(b2)) => eb.eqv(b1, b2)
            case (Left(e1), Left(e2))   => e1.message == e2.message
            case _                      => false
          }
        }
      }
    }
}
