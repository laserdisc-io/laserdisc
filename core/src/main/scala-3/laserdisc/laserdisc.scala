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

import io.github.iltotore.iron.{:|, RefinedSubtype, RuntimeConstraint, refineEither, refineUnsafe}
import io.github.iltotore.iron.constraint.all.*

import scala.annotation.{implicitNotFound, nowarn}
import scala.util.NotGiven

// Basic type aliases
final type |[+A, +B] = Either[A, B]
final type Maybe[A]  = Throwable | A

// Export all protocol
export protocol.{Arr, Bulk, Err, NilArr, NullBulk, Num, Protocol, RESP, RESPDecErr, Read, Show, Str}
final type NilArr   = protocol.NilArr.type
final type NullBulk = protocol.NullBulk.type

// Specialized type lambda
final type ReadArrToSeq[A] = Read[Arr, Seq[A]]

// `|` is Either within laserdisc (see above), so iron's union / intersection constraints are spelled refined-style
private final infix type Or[A, B]  = scala.|[A, B]
private final infix type And[A, B] = A & B

private final type NoControlChar = Not[ControlChar]
private final type NoWhitespace  = Not[WhitespaceChar]

/** Refined-style operations (as found in Scala 2's refined `RefinedTypeOps`) on top of iron's [[RefinedSubtype]], so that shared code
  * can create and deconstruct refined values the same way regardless of the Scala version.
  */
trait RefinedTypeOps[A, C](using RuntimeConstraint[A, C]) extends RefinedSubtype[A, C] {
  final def from(a: A): Either[String, T] = either(a)
  final def unsafeFrom(a: A): T           = applyUnsafe(a)
}

/** Refined-style operations for refined collections, which are plain iron types rather than [[RefinedSubtype]] s */
sealed abstract class RefinedCollectionOps[A, C](using C: RuntimeConstraint[A, C]) {
  final def from(a: A): Either[String, A :| C]    = if (C.test(a)) Right(a.asInstanceOf[A :| C]) else Left(C.message)
  final def unsafeFrom(a: A): A :| C              = from(a).fold(e => throw new IllegalArgumentException(e), identity)
  final def unapply(a: A): Option[A :| C]         = from(a).toOption
}

extension [A, C](a: A :| C) inline def value: A = a

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
private final type IPv4Ref        = Match["^(25[0-5]|2[0-4]\\d|[01]?\\d?\\d)(\\.(25[0-5]|2[0-4]\\d|[01]?\\d?\\d)){3}$"]
private final type Rfc1918Ref     = IPv4Ref And (Match["^10\\..*"] Or Match["^172\\.(16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31)\\..+"] Or Match["^192\\.168\\..*"])
private final type Rfc5737Ref     = IPv4Ref And (Match["^192\\.0\\.2\\..*"] Or Match["^198\\.51\\.100\\..*"] Or Match["^203\\.0\\.113\\..*"])
private final type Rfc3927Ref     = IPv4Ref And Match["^169\\.254\\..*"]
private final type Rfc2544Ref     = IPv4Ref And (Match["^198\\.18\\..*"] Or Match["^198\\.19\\..*"])
private final type ValidDoubleRef = Not[NaN]

final type ConnectionNameRef = MinLength[1] And ForAll[NoWhitespace And NoControlChar]
final type DbIndexRef        = Interval.Closed[0, DbIndexMaxValueWit.type]
final type GeoHashRef        = Match[GeoHashRegexWit.type]
final type GlobPatternRef    = Match[GlobPatternRegexWit.type]
final type HostRef           = StrictEqual[AllNICsEqWit.type] Or StrictEqual[LoopbackEqWit.type] Or
  (Not[IPv4Ref] And MaxLength[Rfc1123HostnameMaxLengthWit.type] And Match[Rfc1123HostnameRegexWit.type]) Or Rfc1918Ref Or Rfc5737Ref Or
  Rfc3927Ref Or Rfc2544Ref
final type IndexRef         = Pure
final type KeyRef           = MinLength[1] And ForAll[NoControlChar]
final type LatitudeRef      = Interval.Closed[LatitudeMinValueWit.type, LatitudeMaxValueWit.type]
final type LongitudeRef     = Interval.Closed[LongitudeMinValueWit.type, LongitudeMaxValueWit.type]
final type NodeIdRef        = Match[NodeIdRegexWit.type]
final type NOKEYRef         = StrictEqual[NOKEYEqWit.type]
final type NonNegRef        = Positive0
final type NonNegDoubleRef  = ValidDoubleRef And Positive0
final type NonZeroDoubleRef = ValidDoubleRef And Not[StrictEqual[0.0d]]
final type NonZeroIntRef    = Not[StrictEqual[0]]
final type NonZeroLongRef   = Not[StrictEqual[0L]]
final type OKRef            = StrictEqual[OKEqWit.type]
final type OneOrMoreRef     = MinLength[1]
final type PONGRef          = StrictEqual[PONGEqWit.type]
final type PortRef          = Interval.Closed[PortMinValueWit.type, PortMaxValueWit.type]
final type PosRef           = Positive
final type RangeOffsetRef   = Interval.Closed[0, RangeOffsetMaxValueWit.type]
final type SlotRef          = Interval.Closed[0, SlotMaxValueWit.type]
final type StringLengthRef  = Interval.Closed[0L, StringLengthMaxValueWit.type]
final type TwoOrMoreRef     = MinLength[2]

// New types
final type ConnectionName        = ConnectionName.T
final type DbIndex               = DbIndex.T
final type GeoHash               = GeoHash.T
final type GlobPattern           = GlobPattern.T
final type Host                  = Host.T
final type Index                 = Index.T
final type Key                   = Key.T
final type Latitude              = Latitude.T
final type Longitude             = Longitude.T
final type NodeId                = NodeId.T
final type NOKEY                 = NOKEYType.T
final type NonNegDouble          = NonNegDouble.T
final type NonNegInt             = NonNegInt.T
final type NonNegLong            = NonNegLong.T
final type NonZeroDouble         = NonZeroDouble.T
final type NonZeroInt            = NonZeroInt.T
final type NonZeroLong           = NonZeroLong.T
final type OK                    = OKType.T
final type OneOrMore[A]          = List[A] :| OneOrMoreRef
final type OneOrMoreKeys         = OneOrMore[Key]
final type PONG                  = PONGType.T
final type Port                  = Port.T
final type PosInt                = PosInt.T
final type PosLong               = PosLong.T
final type RangeOffset           = RangeOffset.T
final type Slot                  = Slot.T
final type StringLength          = StringLength.T
final type TwoOrMoreKeys         = List[Key] :| TwoOrMoreRef
final type TwoOrMoreWeightedKeys = List[(Key, ValidDouble)] :| TwoOrMoreRef
final type ValidDouble           = ValidDouble.T

// New types' ops
object OneOrMore {
  def from[A](l: List[A]): Either[String, OneOrMore[A]] = l.refineEither[OneOrMoreRef]
  def unapply[A](l: List[A]): Option[OneOrMore[A]]      = from(l).toOption
  def unsafeFrom[A](l: List[A]): OneOrMore[A]           = l.refineUnsafe[OneOrMoreRef]
}

object ConnectionName        extends RefinedTypeOps[String, ConnectionNameRef]
object DbIndex               extends RefinedTypeOps[Int, DbIndexRef]
object GeoHash               extends RefinedTypeOps[String, GeoHashRef]
object GlobPattern           extends RefinedTypeOps[String, GlobPatternRef]
object Host                  extends RefinedTypeOps[String, HostRef]
object Index                 extends RefinedTypeOps[Long, IndexRef]
object Key                   extends RefinedTypeOps[String, KeyRef]
object Latitude              extends RefinedTypeOps[Double, LatitudeRef]
object Longitude             extends RefinedTypeOps[Double, LongitudeRef]
object NodeId                extends RefinedTypeOps[String, NodeIdRef]
object NOKEYType             extends RefinedTypeOps[String, NOKEYRef]
object NonNegInt             extends RefinedTypeOps[Int, NonNegRef]
object NonNegLong            extends RefinedTypeOps[Long, NonNegRef]
object NonNegDouble          extends RefinedTypeOps[Double, NonNegDoubleRef]
object NonZeroDouble         extends RefinedTypeOps[Double, NonZeroDoubleRef]
object NonZeroInt            extends RefinedTypeOps[Int, NonZeroIntRef]
object NonZeroLong           extends RefinedTypeOps[Long, NonZeroLongRef]
object OKType                extends RefinedTypeOps[String, OKRef]
object OneOrMoreKeys         extends RefinedCollectionOps[List[Key], OneOrMoreRef]
object PONGType              extends RefinedTypeOps[String, PONGRef]
object Port                  extends RefinedTypeOps[Int, PortRef]
object PosInt                extends RefinedTypeOps[Int, PosRef]
object PosLong               extends RefinedTypeOps[Long, PosRef]
object RangeOffset           extends RefinedTypeOps[Int, RangeOffsetRef]
object Slot                  extends RefinedTypeOps[Int, SlotRef]
object StringLength          extends RefinedTypeOps[Long, StringLengthRef]
object TwoOrMoreKeys         extends RefinedCollectionOps[List[Key], TwoOrMoreRef]
object TwoOrMoreWeightedKeys extends RefinedCollectionOps[List[(Key, ValidDouble)], TwoOrMoreRef]
object ValidDouble           extends RefinedTypeOps[Double, ValidDoubleRef]

final val LoopbackHost: Host = Host(LoopbackEqWit)
final val NOKEY: NOKEY       = NOKEYType("NOKEY")
final val OK: OK             = OKType("OK")
final val PONG: PONG         = PONGType("PONG")

private[laserdisc] final val NodeIdRegex: String = NodeIdRegexWit

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
private[laserdisc] type LUBConstraint[L, A] = Tuple.Union[L & Tuple] <:< A

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
