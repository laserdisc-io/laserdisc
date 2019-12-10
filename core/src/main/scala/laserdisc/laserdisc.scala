import java.{lang => j}

import eu.timepit.refined.W
import eu.timepit.refined.api._
import eu.timepit.refined.boolean.{And, Not, Or, True}
import eu.timepit.refined.char.Whitespace
import eu.timepit.refined.collection.{Forall, MinSize, MaxSize, NonEmpty}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.{Interval, NonNaN, NonNegative, Positive}
import eu.timepit.refined.string.{IPv4, MatchesRegex}
import eu.timepit.refined.types.net.PrivateNetworks._
import shapeless._

package object laserdisc {
  // Basic type aliases
  final type |[A, B]  = Either[A, B]
  final type Maybe[A] = Throwable | A

  // Type forwarders
  final type Arr        = protocol.Arr
  final type Bulk       = protocol.Bulk
  final type Err        = protocol.Err
  final type NilArr     = protocol.NilArr.type
  final type NullBulk   = protocol.NullBulk.type
  final type Num        = protocol.Num
  final type Str        = protocol.Str
  final type Protocol   = protocol.Protocol
  final type ==>[A, B]  = protocol.Read[A, B]
  final type RESP       = protocol.RESP
  final type Show[A]    = protocol.Show[A]
  final type RESPDecErr = protocol.RESPDecErr

  // Object forwarders
  final val Arr        = protocol.Arr
  final val Bulk       = protocol.Bulk
  final val Err        = protocol.Err
  final val NilArr     = protocol.NilArr
  final val NullBulk   = protocol.NullBulk
  final val Num        = protocol.Num
  final val Protocol   = protocol.Protocol
  final val Read       = protocol.Read
  final val Show       = protocol.Show
  final val RESPDecErr = protocol.RESPDecErr

  private[this] final type NoControlChar = Not[ControlChar]
  private[this] final type NoWhitespace  = Not[Whitespace]

  // Witnesses
  final val AllNICsEqWit                = W("0.0.0.0")
  final val DbIndexMaxValueWit          = W(15)
  final val GeoHashRegexWit             = W("[a-z0-9]{11}")
  final val GlobPatternRegexWit         = W("(\\[?[\\w\\*\\?]+\\]?)+") //TODO good enough but needs regex' TLC
  final val IPv4RegexWit                = W("(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)(\\.25[0-5]|2[0-4]\\d|[01]?\\d\\d?){3}")
  final val LatitudeMinValueWit         = W(-85.05112878d)
  final val LatitudeMaxValueWit         = W(85.05112878d)
  final val LongitudeMinValueWit        = W(-180.0d)
  final val LongitudeMaxValueWit        = W(180.0d)
  final val LoopbackEqWit               = W("127.0.0.1")
  final val NodeIdRegexWit              = W("[0-9a-f]{40}")
  final val NOKEYEqWit                  = W("NOKEY")
  final val OKEqWit                     = W("OK")
  final val PONGEqWit                   = W("PONG")
  final val PortMinValueWit             = W(1024)
  final val PortMaxValueWit             = W(49151)
  final val RangeOffsetMaxValueWit      = W(536870911)
  final val Rfc1123HostnameMaxLengthWit = W(255)
  final val Rfc1123HostnameRegexWit = W(
    "(([A-Za-z0-9][\\-A-Za-z0-9]{0,61}[A-Za-z0-9])|[A-Za-z0-9])(\\.(([A-Za-z0-9][\\-A-Za-z0-9]{0,61}[A-Za-z0-9])|[A-Za-z0-9]))*"
  )
  final val SlotMaxValueWit         = W(16383)
  final val StringLengthMaxValueWit = W(4294967295L)

  // Refinements
  final type ConnectionNameRef = OneOrMoreRef And Forall[NoWhitespace And NoControlChar]
  final type DbIndexRef        = Interval.Closed[W.`0`.T, DbIndexMaxValueWit.T]
  final type GeoHashRef        = MatchesRegex[GeoHashRegexWit.T]
  final type GlobPatternRef    = MatchesRegex[GlobPatternRegexWit.T]
  final type HostRef = Equal[AllNICsEqWit.T] Or
    Equal[LoopbackEqWit.T] Or
    (Not[IPv4] And MaxSize[Rfc1123HostnameMaxLengthWit.T] And MatchesRegex[Rfc1123HostnameRegexWit.T]) Or
    Rfc1918PrivateSpec Or
    Rfc5737TestnetSpec Or
    Rfc3927LocalLinkSpec Or
    Rfc2544BenchmarkSpec
  final type IndexRef         = True
  final type KeyRef           = OneOrMoreRef And Forall[NoControlChar]
  final type LatitudeRef      = Interval.Closed[LatitudeMinValueWit.T, LatitudeMaxValueWit.T]
  final type LongitudeRef     = Interval.Closed[LongitudeMinValueWit.T, LongitudeMaxValueWit.T]
  final type NodeIdRef        = MatchesRegex[NodeIdRegexWit.T]
  final type NOKEYRef         = Equal[NOKEYEqWit.T]
  final type NonNegRef        = NonNegative
  final type NonNegDoubleRef  = ValidDoubleRef And NonNegRef
  final type NonZeroDoubleRef = ValidDoubleRef And Not[Equal[W.`0.0D`.T]]
  final type NonZeroIntRef    = Not[Equal[W.`0`.T]]
  final type NonZeroLongRef   = Not[Equal[W.`0L`.T]]
  final type OKRef            = Equal[OKEqWit.T]
  final type OneOrMoreRef     = NonEmpty
  final type PONGRef          = Equal[PONGEqWit.T]
  final type PortRef          = Interval.Closed[PortMinValueWit.T, PortMaxValueWit.T]
  final type PosRef           = Positive
  final type RangeOffsetRef   = Interval.Closed[W.`0`.T, RangeOffsetMaxValueWit.T]
  final type SlotRef          = Interval.Closed[W.`0`.T, SlotMaxValueWit.T]
  final type StringLengthRef  = Interval.Closed[W.`0L`.T, StringLengthMaxValueWit.T]
  final type TwoOrMoreRef     = MinSize[W.`2`.T]
  final type ValidDoubleRef   = NonNaN

  // New types
  final type ConnectionName        = String Refined ConnectionNameRef
  final type DbIndex               = Int Refined DbIndexRef
  final type GeoHash               = String Refined GeoHashRef
  final type GlobPattern           = String Refined GlobPatternRef
  final type Host                  = String Refined HostRef
  final type Index                 = Long Refined IndexRef
  final type Key                   = String Refined KeyRef
  final type Latitude              = Double Refined LatitudeRef
  final type Longitude             = Double Refined LongitudeRef
  final type NodeId                = String Refined NodeIdRef
  final type NOKEY                 = String Refined NOKEYRef
  final type NonNegDouble          = Double Refined NonNegDoubleRef
  final type NonNegInt             = Int Refined NonNegRef
  final type NonNegLong            = Long Refined NonNegRef
  final type NonZeroDouble         = Double Refined NonZeroDoubleRef
  final type NonZeroInt            = Int Refined NonZeroIntRef
  final type NonZeroLong           = Long Refined NonZeroLongRef
  final type OK                    = String Refined OKRef
  final type OneOrMore[A]          = List[A] Refined OneOrMoreRef
  final type OneOrMoreKeys         = OneOrMore[Key]
  final type PONG                  = String Refined PONGRef
  final type Port                  = Int Refined PortRef
  final type PosInt                = Int Refined PosRef
  final type PosLong               = Long Refined PosRef
  final type RangeOffset           = Int Refined RangeOffsetRef
  final type Slot                  = Int Refined SlotRef
  final type StringLength          = Long Refined StringLengthRef
  final type TwoOrMoreKeys         = List[Key] Refined TwoOrMoreRef
  final type TwoOrMoreWeightedKeys = List[(Key, ValidDouble)] Refined TwoOrMoreRef
  final type ValidDouble           = Double Refined ValidDoubleRef

  // New types' ops
  final object OneOrMore {
    def from[A](l: List[A])(implicit rt: RefinedType.AuxT[OneOrMore[A], List[A]]): String | OneOrMore[A] = rt.refine(l)
    def unapply[A](l: List[A]): Option[OneOrMore[A]]                                                     = from(l).toOption
    def unsafeFrom[A](l: List[A])(implicit rt: RefinedType.AuxT[OneOrMore[A], List[A]]): OneOrMore[A]    = rt.unsafeRefine(l)
  }

  final object ConnectionName        extends RefinedTypeOps[ConnectionName, String]
  final object DbIndex               extends RefinedTypeOps.Numeric[DbIndex, Int]
  final object GeoHash               extends RefinedTypeOps[GeoHash, String]
  final object GlobPattern           extends RefinedTypeOps[GlobPattern, String]
  final object Host                  extends RefinedTypeOps[Host, String]
  final object Index                 extends RefinedTypeOps.Numeric[Index, Long]
  final object Key                   extends RefinedTypeOps[Key, String]
  final object Latitude              extends RefinedTypeOps.Numeric[Latitude, Double]
  final object Longitude             extends RefinedTypeOps.Numeric[Longitude, Double]
  final object NodeId                extends RefinedTypeOps[NodeId, String]
  final object NonNegInt             extends RefinedTypeOps.Numeric[NonNegInt, Int]
  final object NonNegLong            extends RefinedTypeOps.Numeric[NonNegLong, Long]
  final object NonNegDouble          extends RefinedTypeOps.Numeric[NonNegDouble, Double]
  final object NonZeroDouble         extends RefinedTypeOps.Numeric[NonZeroDouble, Double]
  final object NonZeroInt            extends RefinedTypeOps.Numeric[NonZeroInt, Int]
  final object NonZeroLong           extends RefinedTypeOps.Numeric[NonZeroLong, Long]
  final object OneOrMoreKeys         extends RefinedTypeOps[OneOrMoreKeys, List[Key]]
  final object Port                  extends RefinedTypeOps.Numeric[Port, Int]
  final object PosInt                extends RefinedTypeOps.Numeric[PosInt, Int]
  final object PosLong               extends RefinedTypeOps.Numeric[PosLong, Long]
  final object RangeOffset           extends RefinedTypeOps.Numeric[RangeOffset, Int]
  final object Slot                  extends RefinedTypeOps.Numeric[Slot, Int]
  final object StringLength          extends RefinedTypeOps.Numeric[StringLength, Long]
  final object TwoOrMoreKeys         extends RefinedTypeOps[TwoOrMoreKeys, List[Key]]
  final object TwoOrMoreWeightedKeys extends RefinedTypeOps[TwoOrMoreWeightedKeys, List[(Key, ValidDouble)]]
  final object ValidDouble           extends RefinedTypeOps.Numeric[ValidDouble, Double]

  final val LoopbackHost: Host = Host.unsafeFrom(LoopbackEqWit.value)
  final val NOKEY: NOKEY       = RefType.applyRefM[NOKEY]("NOKEY")
  final val OK: OK             = RefType.applyRefM[OK]("OK")
  final val PONG: PONG         = RefType.applyRefM[PONG]("PONG")

  private[laserdisc] final val COMMA_CH = ','
  private[laserdisc] final val LF_CH    = '\n'
  private[laserdisc] final val SPACE_CH = ' '
  private[laserdisc] final val COMMA    = s"$COMMA_CH"
  private[laserdisc] final val CRLF     = s"\r$LF_CH"
  private[laserdisc] final val LF       = s"$LF_CH"
  private[laserdisc] final val SPACE    = s"$SPACE_CH"

  private[laserdisc] final object ToInt {
    def unapply(l: Long): Option[Int] =
      try {
        Some(j.Math.toIntExact(l))
      } catch { case _: ArithmeticException => None }
    def unapply(s: String): Option[Int] =
      try {
        Some(j.Integer.parseInt(s))
      } catch { case _: NumberFormatException => None }
  }
  private[laserdisc] final object ToLong {
    def unapply(s: String): Option[Long] =
      try {
        Some(j.Long.parseLong(s))
      } catch { case _: NumberFormatException => None }
  }
  private[laserdisc] final object ToDouble {
    def unapply(s: String): Option[Double] =
      try {
        Some(j.Double.parseDouble(s))
      } catch { case _: NumberFormatException => None }
  }

  private[laserdisc] implicit final class WidenOps1[F[_], A](private val fa: F[A]) extends AnyVal {
    def widen[AA: <:<[A, *]: =:!=[A, *]]: F[AA] = fa.asInstanceOf[F[AA]]
  }

  private[laserdisc] implicit final class WidenOps2[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {
    def widenLeft[AA: <:<[A, *]: =:!=[A, *]]: F[AA, B]                            = fab.asInstanceOf[F[AA, B]]
    def widenRight[BB: <:<[B, *]: =:!=[B, *]]: F[A, BB]                           = fab.asInstanceOf[F[A, BB]]
    def coerceLeft[AA, FF[_, _]](implicit ev: F[AA, B] <:< FF[AA, B]): FF[AA, B]  = fab.asInstanceOf[FF[AA, B]]
    def coerceRight[FF[_, _], BB](implicit ev: F[A, BB] <:< FF[A, BB]): FF[A, BB] = fab.asInstanceOf[FF[A, BB]]
  }

  private[laserdisc] implicit final class WidenOps3[F[_[_], _], G[_], A](private val fga: F[G, A]) extends AnyVal {
    def widenRight[AA: <:<[A, *]: =:!=[A, *]]: F[G, AA] = fga.asInstanceOf[F[G, AA]]
  }
}
