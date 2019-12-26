package laserdisc
package protocol

import cats.instances.int._
import cats.instances.long._
import cats.laws.discipline.{ContravariantTests, MonadTests, SerializableTests}
import cats.{Contravariant, Eq, Monad}
import org.scalacheck.Gen.chooseNum
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.typelevel.discipline.scalatest.Discipline

final class ReadLawsCheck
    extends AnyFunSuiteLike
    with Matchers
    with ScalaCheckDrivenPropertyChecks
    with Discipline
    with Configuration
    with Implicits {

  import ReadInstances._

  checkAll("Read[Num, *]", MonadTests[Read[Num, *]].stackUnsafeMonad[Long, Long, Long])
  checkAll("Monad[Read[Num, *]]", SerializableTests.serializable(Monad[Read[Num, *]]))

  checkAll("Read[*, Int]", ContravariantTests[Read[*, Long]].contravariant[Num, Num, Num])
  checkAll("Contravariant[Read[*, Int]]", SerializableTests.serializable(Contravariant[Read[*, Long]]))
}

private[protocol] sealed trait Implicits {
  implicit val genNum: Gen[Num] = chooseNum(0L, Long.MaxValue) map Num.apply

  implicit val genListOfNum: Gen[List[Num]] = Gen.listOfN(500, genNum) map (_.distinct)

  implicit def arbNum(implicit ev: Gen[Num]): Arbitrary[Num] = Arbitrary(ev)

  implicit val cogenNum: Cogen[Num] = Cogen(_.value)

  implicit def arbPair(implicit ev: Arbitrary[Long]): Arbitrary[(Num, Long)] =
    Arbitrary(ev.arbitrary map (l => Num(l) -> l))

  implicit def arbPairFun(implicit ev: Arbitrary[Long]): Arbitrary[(Num, Long => Long)] =
    Arbitrary(ev.arbitrary map (l => Num(l) -> (_ => l)))

  implicit def arbRead(implicit ev: Arbitrary[Long]): Arbitrary[Read[Num, Long]] =
    Arbitrary(ev.arbitrary map Read.const)

  implicit def arbReadFun(implicit ev: Arbitrary[Long]): Arbitrary[Read[Num, Long => Long]] =
    Arbitrary(ev.arbitrary map (l => Read.const(_ => l)))

  implicit def eqTup[A, B](implicit eb: Eq[B]): Eq[(B, B, B)] =
    new Eq[(B, B, B)] {
      override def eqv(x: (B, B, B), y: (B, B, B)): Boolean =
        (x, y) match {
          case ((x1, x2, x3), (y1, y2, y3)) => eb.eqv(x1, y1) && eb.eqv(x2, y2) && eb.eqv(x3, y3)
          case _                            => false
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
