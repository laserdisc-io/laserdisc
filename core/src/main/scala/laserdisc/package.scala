import java.{lang => j}

import eu.timepit.refined.W
import eu.timepit.refined.auto._
import eu.timepit.refined.api._
import eu.timepit.refined.boolean.{And, Not, Or, True}
import eu.timepit.refined.char.Whitespace
import eu.timepit.refined.collection.{Forall, MinSize, NonEmpty}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.macros.RefineMacro
import eu.timepit.refined.numeric.Interval.{Closed => ClosedInterval}
import eu.timepit.refined.string.{IPv4, MatchesRegex}
import eu.timepit.refined.types.net.PrivateNetworks._
import shapeless._
import shapeless.nat._

package object laserdisc {

  type |[A, B] = Either[A, B]

  //type forwarders
  final type Protocol   = protocol.Protocol
  final type Read[A, B] = protocol.Read[A, B]
  final type RESP       = protocol.RESP
  final type Show[A]    = protocol.Show[A]

  final type OK   = String Refined Equal[W.`"OK"`.T]
  final type PONG = String Refined Equal[W.`"PONG"`.T]

  final val OK: OK     = "OK"
  final val PONG: PONG = "PONG"

  //object forwarders
  final val Protocol = protocol.Protocol
  final val Read     = protocol.Read
  final val Show     = protocol.Show

  final type Maybe[A] = Throwable | A
  final type XString  = String with Singleton

  private[this] final type Loopback = IPv4 And Equal[W.`"127.0.0.1"`.T]
  private[this] final type RFC1123HostName = MatchesRegex[
    W.`"""^([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9])(\\.([a-zA-Z0-9]|[a-zA-Z0-9][a-zA-Z0-9\\-]{0,61}[a-zA-Z0-9]))*$"""`.T]

  implicit final val nanValidator: Validate.Plain[Double, NaN] =
    Validate.fromPredicate(j.Double.isNaN, d => s"($d == NaN)", NaN())

  private[this] final type NonNaN  = Not[NaN]
  private[this] final type NonZero = Not[Equal[_0]]

  //shadowed types
  final type Key        = eu.timepit.refined.types.string.NonEmptyString
  final type NonNegInt  = eu.timepit.refined.types.numeric.NonNegInt
  final type NonNegLong = eu.timepit.refined.types.numeric.NonNegLong
  final type Port       = eu.timepit.refined.types.net.UserPortNumber
  final type PosInt     = eu.timepit.refined.types.numeric.PosInt
  final type PosLong    = eu.timepit.refined.types.numeric.PosLong

  //shadowed types' ops (dispatchers)
  final val Key        = eu.timepit.refined.types.string.NonEmptyString
  final val NonNegInt  = eu.timepit.refined.types.numeric.NonNegInt
  final val NonNegLong = eu.timepit.refined.types.numeric.NonNegLong
  final val Port       = eu.timepit.refined.types.net.UserPortNumber
  final val PosInt     = eu.timepit.refined.types.numeric.PosInt
  final val PosLong    = eu.timepit.refined.types.numeric.PosLong

  //new types
  final type ConnectionName = String Refined (NonEmpty And Forall[Not[Whitespace]])
  final type DbIndex        = Int Refined ClosedInterval[_0, _15]
  final type GlobPattern    = String Refined MatchesRegex[W.`"(\\\\[?[\\\\w\\\\*\\\\?]+\\\\]?)+"`.T] //TODO good enough but needs regex' TLC
  final type Host =
    String Refined (RFC1123HostName Or Loopback Or Rfc1918PrivateSpec Or Rfc5737TestnetSpec Or Rfc3927LocalLinkSpec Or Rfc2544BenchmarkSpec)
  final type Index                      = Long Refined True
  final type NonZeroDouble              = Double Refined (NonNaN And NonZero)
  final type NonZeroInt                 = Int Refined NonZero
  final type NonZeroLong                = Long Refined NonZero
  final type OneOrMore[A]               = List[A] Refined NonEmpty
  final type OneOrMoreKeys              = OneOrMore[Key]
  final type RangeOffset                = Int Refined ClosedInterval[_0, W.`536870911`.T]
  final type SingletonKey[A <: XString] = A Refined NonEmpty
  final type StringLength               = Long Refined ClosedInterval[_0, W.`4294967295L`.T]
  final type TwoOrMoreKeys              = List[Key] Refined MinSize[_2]
  final type TwoOrMoreWeightedKeys      = List[(Key, ValidDouble)] Refined MinSize[_2]
  final type ValidDouble                = Double Refined NonNaN

  //new types' ops
  final object OneOrMore {
    def from[A](l: List[A])(implicit rt: RefinedType.AuxT[OneOrMore[A], List[A]]): String | OneOrMore[A] =
      rt.refine(l)
    def unapply[A](l: List[A]): Option[OneOrMore[A]] = from(l).right.toOption
    def unsafeFrom[A](l: List[A])(implicit rt: RefinedType.AuxT[OneOrMore[A], List[A]]): OneOrMore[A] =
      rt.unsafeRefine(l)
  }
  final object SingletonKey {
    import scala.language.experimental.macros

    def apply[A <: XString](t: A)(
        implicit ev: Refined[A, NonEmpty] =:= SingletonKey[A],
        rt: RefType[Refined],
        v: Validate[A, NonEmpty]
    ): SingletonKey[A] = macro RefineMacro.implApplyRef[SingletonKey[A], Refined, A, NonEmpty]
  }

  final object ConnectionName        extends RefinedTypeOps[ConnectionName, String]
  final object DbIndex               extends RefinedTypeOps[DbIndex, Int]
  final object GlobPattern           extends RefinedTypeOps[GlobPattern, String]
  final object Host                  extends RefinedTypeOps[Host, String]
  final object Index                 extends RefinedTypeOps[Index, Long]
  final object NonZeroDouble         extends RefinedTypeOps[NonZeroDouble, Double]
  final object NonZeroInt            extends RefinedTypeOps[NonZeroInt, Int]
  final object NonZeroLong           extends RefinedTypeOps[NonZeroLong, Long]
  final object OneOrMoreKeys         extends RefinedTypeOps[OneOrMoreKeys, List[Key]]
  final object RangeOffset           extends RefinedTypeOps[RangeOffset, Int]
  final object StringLength          extends RefinedTypeOps[StringLength, Long]
  final object TwoOrMoreKeys         extends RefinedTypeOps[TwoOrMoreKeys, List[Key]]
  final object TwoOrMoreWeightedKeys extends RefinedTypeOps[TwoOrMoreWeightedKeys, List[(Key, ValidDouble)]]
  final object ValidDouble           extends RefinedTypeOps[ValidDouble, Double]

  final object ToInt {
    def unapply(l: Long): Option[Int] =
      try { Some(j.Math.toIntExact(l)) } catch { case _: ArithmeticException => None }
    def unapply(s: String): Option[Int] =
      try { Some(j.Integer.parseInt(s)) } catch { case _: NumberFormatException => None }
  }
  final object ToLong {
    def unapply(s: String): Option[Long] =
      try { Some(j.Long.parseLong(s)) } catch { case _: NumberFormatException => None }
  }
  final object ToDouble {
    def unapply(s: String): Option[Double] =
      try { Some(j.Double.parseDouble(s)) } catch { case _: NumberFormatException => None }
  }

  implicit final class WidenOps1[F[_], A](private val fa: F[A]) extends AnyVal {
    def widen[AA](implicit ev1: A <:< AA, ev2: A =:!= AA): F[AA] = fa.asInstanceOf[F[AA]]
  }

  implicit final class WidenOps2[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {
    def widenLeft[AA](implicit ev1: A <:< AA, ev2: A =:!= AA): F[AA, B]              = fab.asInstanceOf[F[AA, B]]
    def widenRight[BB](implicit ev1: B <:< BB, ev2: B =:!= BB): F[A, BB]             = fab.asInstanceOf[F[A, BB]]
    def widenAsLeftOf[AA, FF[_, _]](implicit ev: F[AA, B] <:< FF[AA, B]): FF[AA, B] = fab.asInstanceOf[FF[AA, B]]
    def widenAsRightOf[FF[_, _], BB](implicit ev: F[A, BB] <:< FF[A, BB]): FF[A, BB]  = fab.asInstanceOf[FF[A, BB]]
  }

  implicit final class WidenOps3[F[_[_], _], G[_], A](private val fga: F[G, A]) extends AnyVal {
    def widenRight[AA](implicit ev1: A <:< AA, ev2: A =:!= AA): F[G, AA] = fga.asInstanceOf[F[G, AA]]
  }
}
