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

import java.{lang => j}

import eu.timepit.refined.api.*
import eu.timepit.refined.boolean.{And, Not, Or, True}
import eu.timepit.refined.char.Whitespace
import eu.timepit.refined.collection.{Forall, MaxSize, MinSize, NonEmpty}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.{Interval, NonNaN, NonNegative, Positive}
import eu.timepit.refined.string.{IPv4, MatchesRegex}
import eu.timepit.refined.types.net.PrivateNetworks.*

import scala.annotation.{implicitNotFound, nowarn}
import scala.util.NotGiven

// Basic type aliases
final type Maybe[A] = Throwable | A

// Export all protocol
export protocol.{Arr, Bulk, Err, NilArr, NullBulk, Num, Protocol, RESP, RESPDecErr, Read, Show, Str}

// Specialized type lambda
final type ReadArrToSeq[A] = Read[Arr, Seq[A]]

private[this] final type NoControlChar = Not[ControlChar]
private[this] final type NoWhitespace  = Not[Whitespace]

// Witnesses
final val AllNICsEqWit                = "0.0.0.0"
final val DbIndexMaxValueWit          = 15
final val GeoHashRegexWit             = "[a-z0-9]{11}"
final val GlobPatternRegexWit         = "(\\[?[\\w\\*\\?]+\\]?)+" // TODO good enough but needs regex' TLC
final val IPv4RegexWit                = "(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)(\\.25[0-5]|2[0-4]\\d|[01]?\\d\\d?){3}"
final val LatitudeMinValueWit         = -85.05112878d
final val LatitudeMaxValueWit         = 85.05112878d
final val LongitudeMinValueWit        = -180.0d
final val LongitudeMaxValueWit        = 180.0d
final val LoopbackEqWit               = "127.0.0.1"
final val NodeIdRegexWit              = "[0-9a-f]{40}"
final val NOKEYEqWit                  = "NOKEY"
final val OKEqWit                     = "OK"
final val PONGEqWit                   = "PONG"
final val PortMinValueWit             = 1024
final val PortMaxValueWit             = 49151
final val RangeOffsetMaxValueWit      = 536870911
final val Rfc1123HostnameMaxLengthWit = 255
final val Rfc1123HostnameRegexWit     =
  "(([A-Za-z0-9][\\-A-Za-z0-9]{0,61}[A-Za-z0-9])|[A-Za-z0-9])(\\.(([A-Za-z0-9][\\-A-Za-z0-9]{0,61}[A-Za-z0-9])|[A-Za-z0-9]))*"
final val SlotMaxValueWit         = 16383
final val StringLengthMaxValueWit = 4294967295L

// Refinements
final type ConnectionNameRef = OneOrMoreRef And Forall[NoWhitespace And NoControlChar]
final type DbIndexRef        = Interval.Closed[0, DbIndexMaxValueWit.type]
final type GeoHashRef        = MatchesRegex[GeoHashRegexWit.type]
final type GlobPatternRef    = MatchesRegex[GlobPatternRegexWit.type]
final type HostRef           = Equal[AllNICsEqWit.type] Or Equal[LoopbackEqWit.type] Or
  (Not[IPv4] And MaxSize[Rfc1123HostnameMaxLengthWit.type] And MatchesRegex[Rfc1123HostnameRegexWit.type]) Or Rfc1918PrivateSpec Or
  Rfc5737TestnetSpec Or Rfc3927LocalLinkSpec Or Rfc2544BenchmarkSpec
final type IndexRef         = True
final type KeyRef           = OneOrMoreRef And Forall[NoControlChar]
final type LatitudeRef      = Interval.Closed[LatitudeMinValueWit.type, LatitudeMaxValueWit.type]
final type LongitudeRef     = Interval.Closed[LongitudeMinValueWit.type, LongitudeMaxValueWit.type]
final type NodeIdRef        = MatchesRegex[NodeIdRegexWit.type]
final type NOKEYRef         = Equal[NOKEYEqWit.type]
final type NonNegRef        = NonNegative
final type NonNegDoubleRef  = ValidDoubleRef And NonNegRef
final type NonZeroDoubleRef = ValidDoubleRef And Not[Equal[0.0d]]
final type NonZeroIntRef    = Not[Equal[0]]
final type NonZeroLongRef   = Not[Equal[0L]]
final type OKRef            = Equal[OKEqWit.type]
final type OneOrMoreRef     = NonEmpty
final type PONGRef          = Equal[PONGEqWit.type]
final type PortRef          = Interval.Closed[PortMinValueWit.type, PortMaxValueWit.type]
final type PosRef           = Positive
final type RangeOffsetRef   = Interval.Closed[0, RangeOffsetMaxValueWit.type]
final type SlotRef          = Interval.Closed[0, SlotMaxValueWit.type]
final type StringLengthRef  = Interval.Closed[0L, StringLengthMaxValueWit.type]
final type TwoOrMoreRef     = MinSize[2]
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
object OneOrMore {
  def from[A](l: List[A])(implicit rt: RefinedType.AuxT[OneOrMore[A], List[A]]): Either[String, OneOrMore[A]] = rt.refine(l)
  def unapply[A](l: List[A]): Option[OneOrMore[A]]                                                            = from(l).toOption
  def unsafeFrom[A](l: List[A])(implicit rt: RefinedType.AuxT[OneOrMore[A], List[A]]): OneOrMore[A]           = rt.unsafeRefine(l)
}

object ConnectionName        extends RefinedTypeOps[ConnectionName, String]
object DbIndex               extends RefinedTypeOps.Numeric[DbIndex, Int]
object GeoHash               extends RefinedTypeOps[GeoHash, String]
object GlobPattern           extends RefinedTypeOps[GlobPattern, String]
object Host                  extends RefinedTypeOps[Host, String]
object Index                 extends RefinedTypeOps.Numeric[Index, Long]
object Key                   extends RefinedTypeOps[Key, String]
object Latitude              extends RefinedTypeOps.Numeric[Latitude, Double]
object Longitude             extends RefinedTypeOps.Numeric[Longitude, Double]
object NodeId                extends RefinedTypeOps[NodeId, String]
object NonNegInt             extends RefinedTypeOps.Numeric[NonNegInt, Int]
object NonNegLong            extends RefinedTypeOps.Numeric[NonNegLong, Long]
object NonNegDouble          extends RefinedTypeOps.Numeric[NonNegDouble, Double]
object NonZeroDouble         extends RefinedTypeOps.Numeric[NonZeroDouble, Double]
object NonZeroInt            extends RefinedTypeOps.Numeric[NonZeroInt, Int]
object NonZeroLong           extends RefinedTypeOps.Numeric[NonZeroLong, Long]
object OneOrMoreKeys         extends RefinedTypeOps[OneOrMoreKeys, List[Key]]
object Port                  extends RefinedTypeOps.Numeric[Port, Int]
object PosInt                extends RefinedTypeOps.Numeric[PosInt, Int]
object PosLong               extends RefinedTypeOps.Numeric[PosLong, Long]
object RangeOffset           extends RefinedTypeOps.Numeric[RangeOffset, Int]
object Slot                  extends RefinedTypeOps.Numeric[Slot, Int]
object StringLength          extends RefinedTypeOps.Numeric[StringLength, Long]
object TwoOrMoreKeys         extends RefinedTypeOps[TwoOrMoreKeys, List[Key]]
object TwoOrMoreWeightedKeys extends RefinedTypeOps[TwoOrMoreWeightedKeys, List[(Key, ValidDouble)]]
object ValidDouble           extends RefinedTypeOps.Numeric[ValidDouble, Double]

final val LoopbackHost: Host = Host.unsafeFrom(LoopbackEqWit)
final val NOKEY: NOKEY       = RefType.applyRef[NOKEY]("NOKEY").getOrElse(absurd)
final val OK: OK             = RefType.applyRef[OK]("OK").getOrElse(absurd)
final val PONG: PONG         = RefType.applyRef[PONG]("PONG").getOrElse(absurd)

private[laserdisc] final val COMMA_CH = ','
private[laserdisc] final val LF_CH    = '\n'
private[laserdisc] final val SPACE_CH = ' '
private[laserdisc] final val COMMA    = s"$COMMA_CH"
private[laserdisc] final val CRLF     = s"\r$LF_CH"
private[laserdisc] final val LF       = s"$LF_CH"
private[laserdisc] final val SPACE    = s"$SPACE_CH"

private[laserdisc] object ToInt {
  def unapply(l: Long): Option[Int] =
    try Some(j.Math.toIntExact(l))
    catch { case _: ArithmeticException => None }
  def unapply(s: String): Option[Int] =
    try Some(j.Integer.parseInt(s))
    catch { case _: NumberFormatException => None }
}
private[laserdisc] object ToLong {
  def unapply(s: String): Option[Long] =
    try Some(j.Long.parseLong(s))
    catch { case _: NumberFormatException => None }
}
private[laserdisc] object ToDouble {
  def unapply(s: String): Option[Double] =
    try Some(j.Double.parseDouble(s))
    catch { case _: NumberFormatException => None }
}

@implicitNotFound("Cannot prove that ${L} can be converted to the tuple ${T}")
type Tupled[L <: Tuple, T] = L =:= T

@implicitNotFound("${A} must not be a subtype of ${B}")
private[laserdisc] type LUBConstraint[L <: Tuple, A] = Tuple.Union[L] <:< A

@implicitNotFound("${A} must not be the same type as ${B}")
private[laserdisc] type =:!=[A, B] = NotGiven[A =:= B]

@implicitNotFound("${A} must not be a subtype of ${B}")
private[laserdisc] type <:!<[A, B] = NotGiven[A <:< B]

private[laserdisc] implicit final class WidenOps1[F[_], A](private val fa: F[A]) extends AnyVal {
  def widen[AA: <:<[A, _]: =:!=[A, _]]: F[AA] = fa.asInstanceOf[F[AA]]
}

private[laserdisc] implicit final class WidenOps2[F[_, _], A, B](private val fab: F[A, B]) extends AnyVal {
  def widenLeft[AA: <:<[A, _]: =:!=[A, _]]: F[AA, B]                                    = fab.asInstanceOf[F[AA, B]]
  def widenRight[BB: <:<[B, _]: =:!=[B, _]]: F[A, BB]                                   = fab.asInstanceOf[F[A, BB]]
  def coerceLeft[AA, FF[_, _]](implicit @nowarn ev: F[AA, B] <:< FF[AA, B]): FF[AA, B]  = fab.asInstanceOf[FF[AA, B]]
  def coerceRight[FF[_, _], BB](implicit @nowarn ev: F[A, BB] <:< FF[A, BB]): FF[A, BB] = fab.asInstanceOf[FF[A, BB]]
}

private[laserdisc] implicit final class WidenOps3[F[_[_], _], G[_], A](private val fga: F[G, A]) extends AnyVal {
  def widenRight[AA: <:<[A, _]: =:!=[A, _]]: F[G, AA] = fga.asInstanceOf[F[G, AA]]
}

private[laserdisc] def absurd: Nothing = throw new RuntimeException("This shouldn't happen. A bug is present in the code")
