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

import io.github.iltotore.iron.{:|, Constraint, RuntimeConstraint, refineEither, refineUnsafe}
import io.github.iltotore.iron.constraint.all.*

import scala.annotation.{implicitNotFound, nowarn}
import scala.util.NotGiven

// Basic type aliases
final type Maybe[A] = Either[Throwable, A]

// Export all protocol
export protocol.{Arr, Bulk, Err, NilArr, NullBulk, Num, Protocol, RESP, RESPDecErr, Read, Show, Str, Value}
final type NilArr   = protocol.NilArr.type
final type NullBulk = protocol.NullBulk.type

// Specialized type lambda
final type ReadArrToSeq[A] = Read[Arr, Seq[A]]

private final type NoControlChar = Not[ControlChar]
private final type NoWhitespace  = Not[WhitespaceChar]

/** A refined newtype `T` (a subtype of `A`), checked by iron's constraint `C`, with the same operations as Scala 2's refined
  * `RefinedTypeOps`: compile-time checked `apply` for literals, `from`, `unsafeFrom`, `unapply` and `.value`, which (as with refined) gives
  * back the base type `A`, so that type class instances for `A` keep working on it.
  */
trait RefinedTypeOps[A, C](using C: RuntimeConstraint[A, C]) {
  opaque type T <: A = A :| C

  inline def apply(inline a: A)(using inline constraint: Constraint[A, C]): T = io.github.iltotore.iron.autoRefine[A, C](a)

  final def from(a: A): Either[String, T] = if (C.test(a)) Right(a.asInstanceOf[T]) else Left(C.message)
  final def unsafeFrom(a: A): T           = from(a).fold(e => throw new IllegalArgumentException(e), identity)
  final def unapply(a: A): Option[T]      = from(a).toOption

  extension (t: T) inline def value: A = t

  given RefinedTypeOps.Mirror[T] with {
    override type BaseType       = A
    override type ConstraintType = C
  }
}
object RefinedTypeOps {

  /** Links a refined newtype to its base type and constraint, used by `auto.autoRefine` */
  trait Mirror[T] {
    type BaseType
    type ConstraintType
  }
}

/** Refined-style operations for refined collections, which are plain iron types rather than [[RefinedTypeOps]] newtypes */
sealed abstract class RefinedCollectionOps[A, C](using C: RuntimeConstraint[A, C]) {
  final def from(a: A): Either[String, A :| C] = if (C.test(a)) Right(a.asInstanceOf[A :| C]) else Left(C.message)
  final def unsafeFrom(a: A): A :| C           = from(a).fold(e => throw new IllegalArgumentException(e), identity)
  final def unapply(a: A): Option[A :| C]      = from(a).toOption
}

extension [A, C](a: A :| C) inline def value: A = a

/** A literal witness, as Scala 2's refined `W` (a shapeless `Witness`): `.value` is the literal, `.T` its literal type */
final class Witness[A <: Singleton] private[laserdisc] (val value: A) {
  type T = A
}

// Witnesses
final val AllNICsEqWit       = new Witness("0.0.0.0")
final val DbIndexMaxValueWit = new Witness(15)
final val GeoHashRegexWit    = new Witness("[a-z0-9]{11}")

/** A Redis glob: `*`, `?`, `[abc]`, `[a-z]`, `[^abc]`, `\x` for the character x itself, or any other character but a control character. A
  * `]` outside of a class, and a `^` first in a class, are written `\]` and `\^`
  */
final val GlobPatternRegexWit = new Witness(
  """[^\x00-\x1F\x7F-\x9F*?\[\]\\]+(?:(?:\\[^\x00-\x1F\x7F-\x9F]|[*?]|\[\^?(?:\\[^\x00-\x1F\x7F-\x9F]|[^\x00-\x1F\x7F-\x9F\]\\^])[^\x00-\x1F\x7F-\x9F\]\\]*(?:\\[^\x00-\x1F\x7F-\x9F][^\x00-\x1F\x7F-\x9F\]\\]*)*\])[^\x00-\x1F\x7F-\x9F*?\[\]\\]*)*|(?:(?:\\[^\x00-\x1F\x7F-\x9F]|[*?]|\[\^?(?:\\[^\x00-\x1F\x7F-\x9F]|[^\x00-\x1F\x7F-\x9F\]\\^])[^\x00-\x1F\x7F-\x9F\]\\]*(?:\\[^\x00-\x1F\x7F-\x9F][^\x00-\x1F\x7F-\x9F\]\\]*)*\])[^\x00-\x1F\x7F-\x9F*?\[\]\\]*)+"""
)
final val IPv4RegexWit                = new Witness("(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)(\\.25[0-5]|2[0-4]\\d|[01]?\\d\\d?){3}")
final val LatitudeMinValueWit         = new Witness(-85.05112878d)
final val LatitudeMaxValueWit         = new Witness(85.05112878d)
final val LongitudeMinValueWit        = new Witness(-180.0d)
final val LongitudeMaxValueWit        = new Witness(180.0d)
final val LoopbackEqWit               = new Witness("127.0.0.1")
final val NodeIdRegexWit              = new Witness("[0-9a-f]{40}")
final val NOKEYEqWit                  = new Witness("NOKEY")
final val OKEqWit                     = new Witness("OK")
final val PONGEqWit                   = new Witness("PONG")
final val PortMinValueWit             = new Witness(1)
final val PortMaxValueWit             = new Witness(65535)
final val RangeOffsetMaxValueWit      = new Witness(536870911)
final val Rfc1123HostnameMaxLengthWit = new Witness(255)
final val Rfc1123HostnameRegexWit     =
  new Witness("(([A-Za-z0-9][\\-A-Za-z0-9]{0,61}[A-Za-z0-9])|[A-Za-z0-9])(\\.(([A-Za-z0-9][\\-A-Za-z0-9]{0,61}[A-Za-z0-9])|[A-Za-z0-9]))*")
final val Sha1RegexWit                = new Witness("[a-f0-9]{40}")
final val SignedBitWidthMaxValueWit   = new Witness(64)
final val SlotMaxValueWit             = new Witness(16383)
final val StringLengthMaxValueWit     = new Witness(4294967295L)
final val UnsignedBitWidthMaxValueWit = new Witness(63)

// Refinements
private final type IPv4Ref    = Match["^(25[0-5]|2[0-4]\\d|[01]?\\d?\\d)(\\.(25[0-5]|2[0-4]\\d|[01]?\\d?\\d)){3}$"]
private final type Rfc1918Ref = IPv4Ref &
  (Match["^10\\..*"] | Match["^172\\.(16|17|18|19|20|21|22|23|24|25|26|27|28|29|30|31)\\..+"] | Match["^192\\.168\\..*"])
private final type Rfc5737Ref     = IPv4Ref & (Match["^192\\.0\\.2\\..*"] | Match["^198\\.51\\.100\\..*"] | Match["^203\\.0\\.113\\..*"])
private final type Rfc3927Ref     = IPv4Ref & Match["^169\\.254\\..*"]
private final type Rfc2544Ref     = IPv4Ref & (Match["^198\\.18\\..*"] | Match["^198\\.19\\..*"])
private final type ValidDoubleRef = NonNaN

final type ConnectionNameRef = MinLength[1] & ForAll[NoWhitespace & NoControlChar]
final type DbIndexRef        = Interval.Closed[0, DbIndexMaxValueWit.T]
final type GeoHashRef        = Match[GeoHashRegexWit.T]
final type GlobPatternRef    = Match[GlobPatternRegexWit.T]
final type HostRef           = StrictEqual[AllNICsEqWit.T] | StrictEqual[LoopbackEqWit.T] |
  (Not[IPv4Ref] & MaxLength[Rfc1123HostnameMaxLengthWit.T] & Match[Rfc1123HostnameRegexWit.T]) | Rfc1918Ref | Rfc5737Ref | Rfc3927Ref |
  Rfc2544Ref
final type IndexRef            = Pure
final type KeyRef              = MinLength[1] & ForAll[NoControlChar]
final type LatitudeRef         = Interval.Closed[LatitudeMinValueWit.T, LatitudeMaxValueWit.T]
final type LongitudeRef        = Interval.Closed[LongitudeMinValueWit.T, LongitudeMaxValueWit.T]
final type NodeIdRef           = Match[NodeIdRegexWit.T]
final type NOKEYRef            = StrictEqual[NOKEYEqWit.T]
final type NonNegRef           = Positive0
final type NonNegDoubleRef     = ValidDoubleRef & Positive0
final type NonZeroDoubleRef    = ValidDoubleRef & Not[StrictEqual[0.0d]]
final type NonZeroIntRef       = Not[StrictEqual[0]]
final type NonZeroLongRef      = Not[StrictEqual[0L]]
final type OKRef               = StrictEqual[OKEqWit.T]
final type OneOrMoreRef        = MinLength[1]
final type PONGRef             = StrictEqual[PONGEqWit.T]
final type PortRef             = Interval.Closed[PortMinValueWit.T, PortMaxValueWit.T]
final type PosRef              = Positive
final type RangeOffsetRef      = Interval.Closed[0, RangeOffsetMaxValueWit.T]
final type Sha1Ref             = Match[Sha1RegexWit.T]
final type SignedBitWidthRef   = Interval.Closed[1, SignedBitWidthMaxValueWit.T]
final type SlotRef             = Interval.Closed[0, SlotMaxValueWit.T]
final type StringLengthRef     = Interval.Closed[0L, StringLengthMaxValueWit.T]
final type UnsignedBitWidthRef = Interval.Closed[1, UnsignedBitWidthMaxValueWit.T]
final type TwoOrMoreRef        = MinLength[2]

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
final type NOKEY                 = String :| NOKEYRef
final type NonNegDouble          = NonNegDouble.T
final type NonNegInt             = NonNegInt.T
final type NonNegLong            = NonNegLong.T
final type NonZeroDouble         = NonZeroDouble.T
final type NonZeroInt            = NonZeroInt.T
final type NonZeroLong           = NonZeroLong.T
final type OK                    = String :| OKRef
final type OneOrMore[A]          = List[A] :| OneOrMoreRef
final type OneOrMoreKeys         = OneOrMore[Key]
final type PONG                  = String :| PONGRef
final type Port                  = Port.T
final type PosInt                = PosInt.T
final type PosLong               = PosLong.T
final type RangeOffset           = RangeOffset.T
final type Sha1                  = Sha1.T
final type SignedBitWidth        = SignedBitWidth.T
final type Slot                  = Slot.T
final type StringLength          = StringLength.T
final type UnsignedBitWidth      = UnsignedBitWidth.T
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
object NonNegInt             extends RefinedTypeOps[Int, NonNegRef]
object NonNegLong            extends RefinedTypeOps[Long, NonNegRef]
object NonNegDouble          extends RefinedTypeOps[Double, NonNegDoubleRef]
object NonZeroDouble         extends RefinedTypeOps[Double, NonZeroDoubleRef]
object NonZeroInt            extends RefinedTypeOps[Int, NonZeroIntRef]
object NonZeroLong           extends RefinedTypeOps[Long, NonZeroLongRef]
object OneOrMoreKeys         extends RefinedCollectionOps[List[Key], OneOrMoreRef]
object Port                  extends RefinedTypeOps[Int, PortRef]
object PosInt                extends RefinedTypeOps[Int, PosRef]
object PosLong               extends RefinedTypeOps[Long, PosRef]
object RangeOffset           extends RefinedTypeOps[Int, RangeOffsetRef]
object Sha1                  extends RefinedTypeOps[String, Sha1Ref]
object SignedBitWidth        extends RefinedTypeOps[Int, SignedBitWidthRef]
object Slot                  extends RefinedTypeOps[Int, SlotRef]
object StringLength          extends RefinedTypeOps[Long, StringLengthRef]
object UnsignedBitWidth      extends RefinedTypeOps[Int, UnsignedBitWidthRef]
object TwoOrMoreKeys         extends RefinedCollectionOps[List[Key], TwoOrMoreRef]
object TwoOrMoreWeightedKeys extends RefinedCollectionOps[List[(Key, ValidDouble)], TwoOrMoreRef]
object ValidDouble           extends RefinedTypeOps[Double, ValidDoubleRef]

final val LoopbackHost: Host = Host.unsafeFrom(LoopbackEqWit.value)
final val NOKEY: NOKEY       = io.github.iltotore.iron.autoRefine[String, NOKEYRef]("NOKEY")
final val OK: OK             = io.github.iltotore.iron.autoRefine[String, OKRef]("OK")
final val PONG: PONG         = io.github.iltotore.iron.autoRefine[String, PONGRef]("PONG")

private[laserdisc] final val NodeIdRegex: String = NodeIdRegexWit.value

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
