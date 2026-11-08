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

import java.lang as j

import eu.timepit.refined.W
import eu.timepit.refined.api.*
import eu.timepit.refined.boolean.{And, Not, Or, True}
import eu.timepit.refined.char.Whitespace
import eu.timepit.refined.collection.{Forall, MaxSize, MinSize, NonEmpty}
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.{Interval, NonNaN, NonNegative, Positive}
import eu.timepit.refined.string.{IPv4, MatchesRegex}
import eu.timepit.refined.types.net.PrivateNetworks.*

import scala.annotation.{implicitNotFound, nowarn}

package object laserdisc {
  type Tuple = shapeless.HList

  type EmptyTuple = shapeless.HNil
  @inline final val EmptyTuple: EmptyTuple = shapeless.HNil

  type NonEmptyTuple       = shapeless.::[?, ?]
  type *:[+A, +B <: Tuple] = shapeless.::[A, B]
  @inline final val *: = shapeless.::

  @implicitNotFound("Cannot prove that ${L} can be converted to the tuple ${T}")
  type Tupled[L <: Tuple, T] = shapeless.ops.hlist.Tupler.Aux[L, T]

  implicit final class TupleOps[T <: Tuple](val t: T) extends AnyVal {
    @inline def *:[H](h: H): H *: T = shapeless.::(h, t)
  }

  @implicitNotFound("Cannot prove that every element of ${L} is a subtype of ${A}")
  type LUBConstraint[L <: Tuple, A] = shapeless.LUBConstraint[L, A]

  // Basic type aliases
  final type Maybe[+A] = Either[Throwable, A]

  // Type forwarders
  final type Arr               = protocol.Arr
  final type Bulk              = protocol.Bulk
  final type Err               = protocol.Err
  final type NilArr            = protocol.NilArr.type
  final type NullBulk          = protocol.NullBulk.type
  final type Num               = protocol.Num
  final type OneOf[T <: Tuple] = protocol.OneOf[T]
  final type Str               = protocol.Str
  final type Protocol[+A]      = protocol.Protocol[A]
  final type Read[A, B]        = protocol.Read[A, B]
  final type RESP              = protocol.RESP
  final type Show[A]           = protocol.Show[A]
  final type RESPDecErr        = protocol.RESPDecErr
  final type Value             = protocol.Value

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
  final val Str        = protocol.Str
  final val RESPDecErr = protocol.RESPDecErr
  final val Value      = protocol.Value

  // a value that may be missing, as RESP2 sends it: a null bulk string for a string, a number or a simple string, and a null array for an
  // array
  final type OrNullBulk[A] = OneOf[NullBulk *: A *: EmptyTuple]
  final type OrNilArr[A]   = OneOf[NilArr *: A *: EmptyTuple]

  private final type NoControlChar = Not[ControlChar]
  private final type NoWhitespace  = Not[Whitespace]

  // Witnesses
  final val AllNICsEqWit               = W("0.0.0.0")
  final val BlockingSecondsMinValueWit = W(0.002d)
  final val BlockingSecondsMaxValueWit = W(4294967.0d)
  final val DbIndexMaxValueWit         = W(2147483647)
  final val GeoHashRegexWit            = W("[a-z0-9]{11}")

  /** A Redis glob: `*`, `?`, `[abc]`, `[a-z]`, `[^abc]`, `\x` for the character x itself, or any other character but a control character. A
    * `]` outside of a class, and a `^` first in a class, are written `\]` and `\^`
    */
  final val GlobPatternRegexWit = W(
    """[^\x00-\x1F\x7F-\x9F*?\[\]\\]+(?:(?:\\[^\x00-\x1F\x7F-\x9F]|[*?]|\[\^?(?:\\[^\x00-\x1F\x7F-\x9F]|[^\x00-\x1F\x7F-\x9F\]\\^])[^\x00-\x1F\x7F-\x9F\]\\]*(?:\\[^\x00-\x1F\x7F-\x9F][^\x00-\x1F\x7F-\x9F\]\\]*)*\])[^\x00-\x1F\x7F-\x9F*?\[\]\\]*)*|(?:(?:\\[^\x00-\x1F\x7F-\x9F]|[*?]|\[\^?(?:\\[^\x00-\x1F\x7F-\x9F]|[^\x00-\x1F\x7F-\x9F\]\\^])[^\x00-\x1F\x7F-\x9F\]\\]*(?:\\[^\x00-\x1F\x7F-\x9F][^\x00-\x1F\x7F-\x9F\]\\]*)*\])[^\x00-\x1F\x7F-\x9F*?\[\]\\]*)+"""
  )

  /** A glob for the keys and channels of ACL rules: a [[GlobPattern]] without spaces, which ACL SETUSER refuses */
  final val AclPatternRegexWit = W(
    """[^ \x00-\x1F\x7F-\x9F*?\[\]\\]+(?:(?:\\[^ \x00-\x1F\x7F-\x9F]|[*?]|\[\^?(?:\\[^ \x00-\x1F\x7F-\x9F]|[^ \x00-\x1F\x7F-\x9F\]\\^])[^ \x00-\x1F\x7F-\x9F\]\\]*(?:\\[^ \x00-\x1F\x7F-\x9F][^ \x00-\x1F\x7F-\x9F\]\\]*)*\])[^ \x00-\x1F\x7F-\x9F*?\[\]\\]*)*|(?:(?:\\[^ \x00-\x1F\x7F-\x9F]|[*?]|\[\^?(?:\\[^ \x00-\x1F\x7F-\x9F]|[^ \x00-\x1F\x7F-\x9F\]\\^])[^ \x00-\x1F\x7F-\x9F\]\\]*(?:\\[^ \x00-\x1F\x7F-\x9F][^ \x00-\x1F\x7F-\x9F\]\\]*)*\])[^ \x00-\x1F\x7F-\x9F*?\[\]\\]*)+"""
  )
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
  final val PortMinValueWit             = W(1)
  final val PortMaxValueWit             = W(65535)
  final val RangeOffsetMaxValueWit      = W(536870911)
  final val Rfc1123HostnameMaxLengthWit = W(255)
  final val Rfc1123HostnameRegexWit     = W(
    "(([A-Za-z0-9][\\-A-Za-z0-9]{0,61}[A-Za-z0-9])|[A-Za-z0-9])(\\.(([A-Za-z0-9][\\-A-Za-z0-9]{0,61}[A-Za-z0-9])|[A-Za-z0-9]))*"
  )
  final val Sha1RegexWit                = W("[a-f0-9]{40}")
  final val SignedBitWidthMaxValueWit   = W(64)
  final val SlotMaxValueWit             = W(16383)
  final val StringLengthMaxValueWit     = W(4294967295L)
  final val UnsignedBitWidthMaxValueWit = W(63)

  // Refinements
  /** Seconds a blocking command waits, 0 to wait forever. Not below 2ms, as KeyDB and Dragonfly wait forever on less, nor above what
    * Dragonfly takes
    */
  final type BlockingSecondsRef = NonNaN And
    (Equal[W.`0.0D`.T] Or Interval.Closed[BlockingSecondsMinValueWit.T, BlockingSecondsMaxValueWit.T])
  final type ConnectionNameRef = OneOrMoreRef And Forall[NoWhitespace And NoControlChar]
  final type DbIndexRef        = Interval.Closed[W.`0`.T, DbIndexMaxValueWit.T]
  final type GeoHashRef        = MatchesRegex[GeoHashRegexWit.T]
  final type GlobPatternRef    = MatchesRegex[GlobPatternRegexWit.T]
  final type AclPatternRef     = MatchesRegex[AclPatternRegexWit.T]
  final type HostRef           = Equal[AllNICsEqWit.T] Or Equal[LoopbackEqWit.T] Or
    (Not[IPv4] And MaxSize[Rfc1123HostnameMaxLengthWit.T] And MatchesRegex[Rfc1123HostnameRegexWit.T]) Or Rfc1918PrivateSpec Or
    Rfc5737TestnetSpec Or Rfc3927LocalLinkSpec Or Rfc2544BenchmarkSpec
  final type IndexRef            = True
  final type KeyRef              = OneOrMoreRef And Forall[NoControlChar]
  final type LatitudeRef         = NonNaN And Interval.Closed[LatitudeMinValueWit.T, LatitudeMaxValueWit.T]
  final type LongitudeRef        = NonNaN And Interval.Closed[LongitudeMinValueWit.T, LongitudeMaxValueWit.T]
  final type NodeIdRef           = MatchesRegex[NodeIdRegexWit.T]
  final type NOKEYRef            = Equal[NOKEYEqWit.T]
  final type NonNegRef           = NonNegative
  final type NonNegDoubleRef     = ValidDoubleRef And NonNegRef
  final type NonZeroDoubleRef    = ValidDoubleRef And Not[Equal[W.`0.0D`.T]]
  final type NonZeroIntRef       = Not[Equal[W.`0`.T]]
  final type NonZeroLongRef      = Not[Equal[W.`0L`.T]]
  final type OKRef               = Equal[OKEqWit.T]
  final type OneOrMoreRef        = NonEmpty
  final type PONGRef             = Equal[PONGEqWit.T]
  final type PortRef             = Interval.Closed[PortMinValueWit.T, PortMaxValueWit.T]
  final type PosRef              = Positive
  final type RangeOffsetRef      = Interval.Closed[W.`0`.T, RangeOffsetMaxValueWit.T]
  final type Sha1Ref             = MatchesRegex[Sha1RegexWit.T]
  final type SignedBitWidthRef   = Interval.Closed[W.`1`.T, SignedBitWidthMaxValueWit.T]
  final type SlotRef             = Interval.Closed[W.`0`.T, SlotMaxValueWit.T]
  final type StringLengthRef     = Interval.Closed[W.`0L`.T, StringLengthMaxValueWit.T]
  final type UnsignedBitWidthRef = Interval.Closed[W.`1`.T, UnsignedBitWidthMaxValueWit.T]
  final type TwoOrMoreRef        = MinSize[W.`2`.T]
  final type ValidDoubleRef      = NonNaN

  // New types
  final type BlockingSeconds       = Double Refined BlockingSecondsRef
  final type ConnectionName        = String Refined ConnectionNameRef
  final type DbIndex               = Int Refined DbIndexRef
  final type GeoHash               = String Refined GeoHashRef
  final type GlobPattern           = String Refined GlobPatternRef
  final type AclPattern            = String Refined AclPatternRef
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
  final type Sha1                  = String Refined Sha1Ref
  final type SignedBitWidth        = Int Refined SignedBitWidthRef
  final type Slot                  = Int Refined SlotRef
  final type StringLength          = Long Refined StringLengthRef
  final type TwoOrMoreKeys         = List[Key] Refined TwoOrMoreRef
  final type TwoOrMoreWeightedKeys = List[(Key, ValidDouble)] Refined TwoOrMoreRef
  final type UnsignedBitWidth      = Int Refined UnsignedBitWidthRef
  final type ValidDouble           = Double Refined ValidDoubleRef

  // New types' ops
  object OneOrMore {
    def from[A](l: List[A])(implicit rt: RefinedType.AuxT[OneOrMore[A], List[A]]): Either[String, OneOrMore[A]] = rt.refine(l)
    def unapply[A](l: List[A]): Option[OneOrMore[A]]                                                            = from(l).toOption
    def unsafeFrom[A](l: List[A])(implicit rt: RefinedType.AuxT[OneOrMore[A], List[A]]): OneOrMore[A]           = rt.unsafeRefine(l)
  }

  object BlockingSeconds       extends RefinedTypeOps[BlockingSeconds, Double]
  object ConnectionName        extends RefinedTypeOps[ConnectionName, String]
  object DbIndex               extends RefinedTypeOps.Numeric[DbIndex, Int]
  object GeoHash               extends RefinedTypeOps[GeoHash, String]
  object GlobPattern           extends RefinedTypeOps[GlobPattern, String]
  object AclPattern            extends RefinedTypeOps[AclPattern, String]
  object Host                  extends RefinedTypeOps[Host, String]
  object Index                 extends RefinedTypeOps.Numeric[Index, Long]
  object Key                   extends RefinedTypeOps[Key, String]
  object Latitude              extends RefinedTypeOps[Latitude, Double]
  object Longitude             extends RefinedTypeOps[Longitude, Double]
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
  object Sha1                  extends RefinedTypeOps[Sha1, String]
  object SignedBitWidth        extends RefinedTypeOps.Numeric[SignedBitWidth, Int]
  object Slot                  extends RefinedTypeOps.Numeric[Slot, Int]
  object StringLength          extends RefinedTypeOps.Numeric[StringLength, Long]
  object TwoOrMoreKeys         extends RefinedTypeOps[TwoOrMoreKeys, List[Key]]
  object TwoOrMoreWeightedKeys extends RefinedTypeOps[TwoOrMoreWeightedKeys, List[(Key, ValidDouble)]]
  object UnsignedBitWidth      extends RefinedTypeOps.Numeric[UnsignedBitWidth, Int]
  object ValidDouble           extends RefinedTypeOps.Numeric[ValidDouble, Double]

  final val LoopbackHost: Host = Host.unsafeFrom(LoopbackEqWit.value)
  final val NOKEY: NOKEY       = RefType.applyRefM[NOKEY]("NOKEY")
  final val OK: OK             = RefType.applyRefM[OK]("OK")
  final val PONG: PONG         = RefType.applyRefM[PONG]("PONG")

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

  @nowarn private[laserdisc] sealed trait =:!=[A, B] extends Serializable
  private[laserdisc] implicit def neq[A, B]: A =:!= B    = new =:!=[A, B] {}
  private[laserdisc] implicit def neqAmbig1[A]: A =:!= A = absurd
  private[laserdisc] implicit def neqAmbig2[A]: A =:!= A = absurd

  @implicitNotFound("${A} must not be a subtype of ${B}")
  @nowarn private[laserdisc] sealed trait <:!<[A, B] extends Serializable

  private[laserdisc] implicit def nsub[A, B]: A <:!< B            = new <:!<[A, B] {}
  private[laserdisc] implicit def nsubAmbig1[A, B >: A]: A <:!< B = absurd
  private[laserdisc] implicit def nsubAmbig2[A, B >: A]: A <:!< B = absurd

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
}
