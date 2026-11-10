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

import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.*
import org.scalacheck.{Arbitrary, Gen}

import scala.Double.{MaxValue as DMax, MinValue as DMin, NaN}
import scala.Int.{MaxValue as IMax, MinValue as IMin}
import scala.Long.{MaxValue as LMax, MinValue as LMin}

abstract class BaseSpec extends ScalaCheckSuite with ScalaCheckSettings with BaseSpecPlatform {

  protected final type EmptyString = TestTypes.EmptyString
  protected final val EmptyString: EmptyString = TestTypes.EmptyString

  private final val dashChar: Char     = 0x002d.toChar
  private final val dashString: String = dashChar.toString
  private final val dotString: String  = 0x002e.toChar.toString
  private final val spaceChar: Char    = 0x0020.toChar

  private final val byteRange: Gen[Int]         = chooseNum(0, 255)
  private final val dashGen: Gen[Char]          = const(dashChar)
  private final val hexGen: Gen[Char]           = frequency(10 -> numChar, 6 -> choose(0x0061.toChar, 0x0066.toChar))
  private final val spaceGen: Gen[Char]         = const(spaceChar)
  protected final val utf8BMPCharGen: Gen[Char] = {
    val b01 = 24 -> choose(spaceChar, 0x007e.toChar)     // 75% it's a 7-bit ASCII char
    val b02 = 1  -> choose(0x00a0.toChar, 0x085f.toChar) // 3.125% for all other cases
    val b03 = 1  -> choose(0x08a0.toChar, 0x1aaf.toChar)
    val b04 = 1  -> choose(0x1b00.toChar, 0x1c7f.toChar)
    val b05 = 1  -> choose(0x1cc0.toChar, 0x2fdf.toChar)
    val b06 = 1  -> choose(0x2ff0.toChar, 0xa9df.toChar)
    val b07 = 1  -> choose(0xaa00.toChar, 0xab2f.toChar)
    val b08 = 1  -> choose(0xabc0.toChar, 0xd7ff.toChar)
    val b09 = 1  -> choose(0xe000.toChar, 0xffef.toChar)

    frequency(b01, b02, b03, b04, b05, b06, b07, b08, b09)
  }
  private final val noSpaceUtf8BMPCharGen: Gen[Char] = utf8BMPCharGen.suchThat(_ != spaceChar)

  private final val globGen: Gen[String] = {
    val escaped   = Gen.oneOf("\\*", "\\?", "\\[", "\\]", "\\\\", "\\^")
    val plain     = alphaNumChar.map(_.toString)
    val basicGlob = nonEmptyListOf(frequency(1 -> const("*"), 1 -> const("?"), 1 -> const(":"), 1 -> const("-"), 1 -> escaped, 4 -> plain))
    val classBody = nonEmptyListOf(frequency(1 -> const("-"), 1 -> escaped, 4 -> plain)).map(_.mkString)
    frequency(3 -> basicGlob.map(_.mkString), 1 -> classBody.map(c => s"[$c]"), 1 -> classBody.map(c => s"[^$c]"))
  }
  private final val allNICsGen: Gen[String] = const(AllNICsEqWit.value)
  private final val lbGen: Gen[String]      = const(LoopbackEqWit.value)
  private final val rfc1123Gen: Gen[String] = {
    def dashAtBeginOrEnd(s: String) = s.startsWith(dashString) || s.endsWith(dashString)
    val piece                       = chooseNum(1, 62).flatMap(strOfNGen(_, 1 -> dashGen, 99 -> alphaNumChar))
    choose(1, 5).flatMap(listOfN(_, piece.retryUntil(!dashAtBeginOrEnd(_))).map(_.mkString(dotString)).retryUntil(_.size <= 255))
  }
  private final val rfc1918Gen: Gen[String] = Gen.oneOf(ipv4Gen(10), chooseNum(16, 31).flatMap(ipv4Gen(172, _)), ipv4Gen(192, 168))
  private final val rfc5737Gen: Gen[String] = Gen.oneOf(ipv4Gen(192, 0, 2), ipv4Gen(198, 51, 100), ipv4Gen(203, 0, 113))
  private final val rfc3927Gen: Gen[String] = ipv4Gen(169, 254)
  private final val rfc2544Gen: Gen[String] = chooseNum(18, 19).flatMap(ipv4Gen(198, _))

  private final def ipv4Gen(hs: Int*): Gen[String]           = listOfN(4 - hs.size, byteRange).map(ts => (hs ++: ts).mkString(dotString))
  private final def twoOrMore[A](ga: =>Gen[A]): Gen[List[A]] = nonEmptyListOf(ga).suchThat(_.size > 1)
  private final def zip[A, B](arbA: =>Arbitrary[A], arbB: =>Arbitrary[B]): Gen[(A, B)] =
    arbA.arbitrary.flatMap(a => arbB.arbitrary.map(a -> _))

  private final def strGen(lc: Gen[List[Char]]): Gen[String]               = lc.map(_.mkString)
  private final def strOfNGen(n: Int, fs: (Int, Gen[Char])*): Gen[String]  = listOfN(n, frequency(fs*)).map(_.mkString)
  private final def strOfNSameFreqGen(n: Int, gs: Gen[Char]*): Gen[String] = strOfNGen(n, gs.map(1 -> _)*)

  final val blockingSecondsIsValid: Double => Boolean                         = BlockingSeconds.from(_).isRight
  final val connectionNameIsValid: String => Boolean                          = ConnectionName.from(_).isRight
  final val dbIndexIsValid: Int => Boolean                                    = DbIndex.from(_).isRight
  final val signedBitWidthIsValid: Int => Boolean                             = SignedBitWidth.from(_).isRight
  final val unsignedBitWidthIsValid: Int => Boolean                           = UnsignedBitWidth.from(_).isRight
  final val geoHashIsValid: String => Boolean                                 = GeoHash.from(_).isRight
  final val globPatternIsValid: String => Boolean                             = GlobPattern.from(_).isRight
  final val hostIsValid: String => Boolean                                    = Host.from(_).isRight
  final val keyIsValid: String => Boolean                                     = Key.from(_).isRight
  final val latitudeIsValid: Double => Boolean                                = Latitude.from(_).isRight
  final val longitudeIsValid: Double => Boolean                               = Longitude.from(_).isRight
  final val nodeIdIsValid: String => Boolean                                  = NodeId.from(_).isRight
  final val sha1IsValid: String => Boolean                                    = Sha1.from(_).isRight
  final val nonNegDoubleIsValid: Double => Boolean                            = NonNegDouble.from(_).isRight
  final val nonNegIntIsValid: Int => Boolean                                  = NonNegInt.from(_).isRight
  final val nonNegLongIsValid: Long => Boolean                                = NonNegLong.from(_).isRight
  final val nonZeroDoubleIsValid: Double => Boolean                           = NonZeroDouble.from(_).isRight
  final val nonZeroIntIsValid: Int => Boolean                                 = NonZeroInt.from(_).isRight
  final val nonZeroLongIsValid: Long => Boolean                               = NonZeroLong.from(_).isRight
  final def oneOrMoreIsValid[A]: List[A] => Boolean                           = OneOrMore.from(_).isRight
  final val oneOrMoreKeysIsValid: List[Key] => Boolean                        = OneOrMoreKeys.from(_).isRight
  final val portIsValid: Int => Boolean                                       = Port.from(_).isRight
  final val rangeOffsetIsValid: Int => Boolean                                = RangeOffset.from(_).isRight
  final val slotIsValid: Int => Boolean                                       = Slot.from(_).isRight
  final val stringLengthIsValid: Long => Boolean                              = StringLength.from(_).isRight
  final val twoOrMoreKeysIsValid: List[Key] => Boolean                        = TwoOrMoreKeys.from(_).isRight
  final val twoOrMoreWeightedKeysIsValid: List[(Key, ValidDouble)] => Boolean = TwoOrMoreWeightedKeys.from(_).isRight
  final val validDoubleIsValid: Double => Boolean                             = ValidDouble.from(_).isRight

  final val blockingSecondsGen: Gen[Double] =
    frequency(1 -> const(0.0d), 9 -> chooseNum(BlockingSecondsMinValueWit.value, BlockingSecondsMaxValueWit.value)) :| "blocking seconds"
  final val connectionNameGen: Gen[String] =
    strGen(nonEmptyListOf(noSpaceUtf8BMPCharGen)).filter(connectionNameIsValid) :| "connection name"
  final val dbIndexGen: Gen[Int]        = chooseNum(0, DbIndexMaxValueWit.value).filter(dbIndexIsValid) :| "db index"
  final val signedBitWidthGen: Gen[Int] = chooseNum(1, SignedBitWidthMaxValueWit.value).filter(signedBitWidthIsValid) :| "signed bit width"
  final val unsignedBitWidthGen: Gen[Int] =
    chooseNum(1, UnsignedBitWidthMaxValueWit.value).filter(unsignedBitWidthIsValid) :| "unsigned bit width"
  final val directionGen: Gen[Direction] = Gen.oneOf(Direction.asc, Direction.desc) :| "direction"
  final val geoHashGen: Gen[String]      = strOfNGen(11, 1 -> numChar, 9 -> alphaLowerChar).filter(geoHashIsValid) :| "geo hash"
  final val globPatternGen: Gen[String]  =
    Gen.resize(20, nonEmptyListOf(globGen)).map(_.mkString).filter(globPatternIsValid) :| "glob pattern"
  final val hostGen: Gen[Host] = {
    val hosts = 7 -> Gen
      .oneOf(allNICsGen, lbGen, rfc1123Gen, rfc1918Gen, rfc5737Gen, rfc3927Gen, rfc2544Gen)
      .filter(hostIsValid)
      .map(hs => Host.unsafeFrom(hs))
    val loopbackHost = 1 -> Gen.const(LoopbackHost)

    frequency(hosts, loopbackHost)
  } :| "host"
  final val hostOrEmptyGen: Gen[Either[EmptyString, Host]] =
    frequency(10 -> hostGen.map(Right.apply), 1 -> const(EmptyString).map(Left.apply)) :| "host or empty string"
  final val keyGen: Gen[Key]         = strGen(nonEmptyListOf(utf8BMPCharGen)).filter(keyIsValid).map(Key.unsafeFrom) :| "key"
  final val latitudeGen: Gen[Double] = chooseNum(LatitudeMinValueWit.value, LatitudeMaxValueWit.value).filter(latitudeIsValid) :| "latitude"
  final val longitudeGen: Gen[Double] =
    chooseNum(LongitudeMinValueWit.value, LongitudeMaxValueWit.value).filter(longitudeIsValid) :| "longitude"
  final val nodeIdGen: Gen[String]        = strOfNSameFreqGen(40, hexGen).filter(nodeIdIsValid) :| "node id"
  final val sha1Gen: Gen[String]          = strOfNSameFreqGen(40, hexGen).filter(sha1IsValid) :| "sha1"
  final val nonNegDoubleGen: Gen[Double]  = chooseNum(0.0d, DMax).filter(nonNegDoubleIsValid) :| "double >= 0.0D"
  final val nonNegIntGen: Gen[Int]        = chooseNum(0, IMax).filter(nonNegIntIsValid) :| "int >= 0"
  final val nonNegLongGen: Gen[Long]      = chooseNum(0L, LMax).filter(nonNegLongIsValid) :| "long >= 0L"
  final val nonZeroDoubleGen: Gen[Double] =
    chooseNum(DMin, DMax).suchThat(d => d != 0.0d && d != NaN).filter(nonZeroDoubleIsValid) :| "double != 0.0D and != NaN"
  final val nonZeroIntGen: Gen[Int]           = chooseNum(IMin, IMax).suchThat(_ != 0).filter(nonZeroIntIsValid) :| "int != 0"
  final val nonZeroLongGen: Gen[Long]         = chooseNum(LMin, LMax).suchThat(_ != 0L).filter(nonZeroLongIsValid) :| "long != 0L"
  final val portGen: Gen[Port]                = chooseNum(PortMinValueWit.value, PortMaxValueWit.value).map(Port.unsafeFrom) :| "port"
  final val rangeOffsetGen: Gen[Int]          = chooseNum(0, RangeOffsetMaxValueWit.value).filter(rangeOffsetIsValid) :| "range offset"
  final val slotGen: Gen[Slot]                = chooseNum(0, SlotMaxValueWit.value).map(Slot.unsafeFrom) :| "slot"
  final val stringLengthGen: Gen[Long]        = chooseNum(0L, StringLengthMaxValueWit.value).filter(stringLengthIsValid) :| "string length"
  final val stringsWithSpacesGen: Gen[String] =
    strGen(listOf(frequency(1 -> spaceGen, 10 -> noSpaceUtf8BMPCharGen))).filterNot(connectionNameIsValid) :| "string w/ spaces"
  final val validDoubleGen: Gen[Double] = chooseNum(DMin, DMax).suchThat(_ != NaN) :| "double != NaN"

  implicit final val blockingSecondsArb: Arbitrary[BlockingSeconds]       = Arbitrary(blockingSecondsGen.map(BlockingSeconds.unsafeFrom))
  implicit final val connectionNameArb: Arbitrary[ConnectionName]         = Arbitrary(connectionNameGen.map(ConnectionName.unsafeFrom))
  implicit final val directionArb: Arbitrary[Direction]                   = Arbitrary(directionGen)
  implicit final val geoHashArb: Arbitrary[GeoHash]                       = Arbitrary(geoHashGen.map(GeoHash.unsafeFrom))
  implicit final val globPatternArb: Arbitrary[GlobPattern]               = Arbitrary(globPatternGen.map(GlobPattern.unsafeFrom))
  implicit final val hostArb: Arbitrary[Host]                             = Arbitrary(hostGen)
  implicit final val indexArb: Arbitrary[Index]                           = Arbitrary(arbitrary[Long].map(Index.unsafeFrom))
  implicit final val keyArb: Arbitrary[Key]                               = Arbitrary(keyGen)
  implicit final def kvArb[A](implicit A: Arbitrary[A]): Arbitrary[KV[A]] =
    Arbitrary(keyArb.arbitrary.flatMap(k => A.arbitrary.map(KV(k, _))))
  implicit final val nodeIdArb: Arbitrary[NodeId]               = Arbitrary(nodeIdGen.map(NodeId.unsafeFrom))
  implicit final val nonNegDoubleArb: Arbitrary[NonNegDouble]   = Arbitrary(nonNegDoubleGen.map(NonNegDouble.unsafeFrom))
  implicit final val nonNegIntArb: Arbitrary[NonNegInt]         = Arbitrary(nonNegIntGen.map(NonNegInt.unsafeFrom))
  implicit final val nonNegLongArb: Arbitrary[NonNegLong]       = Arbitrary(nonNegLongGen.map(NonNegLong.unsafeFrom))
  implicit final val nonZeroDoubleArb: Arbitrary[NonZeroDouble] = Arbitrary(nonZeroDoubleGen.map(NonZeroDouble.unsafeFrom))
  implicit final val nonZeroIntArb: Arbitrary[NonZeroInt]       = Arbitrary(nonZeroIntGen.map(NonZeroInt.unsafeFrom))
  implicit final val nonZeroLongArb: Arbitrary[NonZeroLong]     = Arbitrary(nonZeroLongGen.map(NonZeroLong.unsafeFrom))
  implicit final val scanKeyArb: Arbitrary[Scan[Key]]           =
    Arbitrary(nonNegLongArb.arbitrary.flatMap(l => option(listOf(keyArb.arbitrary)).map(Scan(l, _))))
  implicit final val scanKVArb: Arbitrary[ScanKV] =
    Arbitrary(nonNegLongArb.arbitrary.flatMap(l => option(listOf(kvArb[String].arbitrary)).map(ScanKV(l, _))))
  implicit final val slotArb: Arbitrary[Slot]                   = Arbitrary(slotGen)
  implicit final val twoOrMoreKeysArb: Arbitrary[TwoOrMoreKeys] = Arbitrary(twoOrMore(keyArb.arbitrary).map(TwoOrMoreKeys.unsafeFrom))
  implicit final val twoOrMoreWeightedKeysArb: Arbitrary[TwoOrMoreWeightedKeys] = Arbitrary(
    twoOrMore(zip(keyArb, validDoubleArb)).map(TwoOrMoreWeightedKeys.unsafeFrom)
  )
  implicit final val dbIndexArb: Arbitrary[DbIndex]                   = Arbitrary(dbIndexGen.map(DbIndex.unsafeFrom))
  implicit final val signedBitWidthArb: Arbitrary[SignedBitWidth]     = Arbitrary(signedBitWidthGen.map(SignedBitWidth.unsafeFrom))
  implicit final val unsignedBitWidthArb: Arbitrary[UnsignedBitWidth] = Arbitrary(unsignedBitWidthGen.map(UnsignedBitWidth.unsafeFrom))
  implicit final val latitudeArb: Arbitrary[Latitude]                 = Arbitrary(latitudeGen.map(Latitude.unsafeFrom))
  implicit final val longitudeArb: Arbitrary[Longitude]               = Arbitrary(longitudeGen.map(Longitude.unsafeFrom))
  implicit final def oneOrMoreArb[A](implicit A: Arbitrary[A]): Arbitrary[OneOrMore[A]] =
    Arbitrary(nonEmptyListOf(A.arbitrary).map(OneOrMore.unsafeFrom))
  implicit final val portArb: Arbitrary[Port]                 = Arbitrary(portGen)
  implicit final val posIntArb: Arbitrary[PosInt]             = Arbitrary(chooseNum(1, IMax).map(PosInt.unsafeFrom))
  implicit final val posLongArb: Arbitrary[PosLong]           = Arbitrary(chooseNum(1L, LMax).map(PosLong.unsafeFrom))
  implicit final val rangeOffsetArb: Arbitrary[RangeOffset]   = Arbitrary(rangeOffsetGen.map(RangeOffset.unsafeFrom))
  implicit final val stringLengthArb: Arbitrary[StringLength] = Arbitrary(stringLengthGen.map(StringLength.unsafeFrom))
  implicit final val validDoubleArb: Arbitrary[ValidDouble]   = Arbitrary(validDoubleGen.map(ValidDouble.unsafeFrom))

  final val boolToNum: Boolean => Num = b => Num(if (b) 1 else 0)

  private[laserdisc] def assertEquals[A, B](eab: Either[A, B], b: B): Unit =
    eab.fold(err => fail(s"It Should be right but was left with $err"), r => assertEquals(r, b))

  private[laserdisc] def assertLeftEquals[A, B](eab: Either[A, B], a: A): Unit =
    eab.fold(l => assertEquals(l, a), res => fail(s"It Should be left but was right with $res"))

  private[laserdisc] def fails[A, B](eab: Either[A, B], a: A): Unit =
    eab.fold(e => assertEquals(e, a), res => fail(s"It Should be left but was right with $res"))

  final val succeed: Unit = assert(cond = true)

  /** The reply of a command that answers with a number, or with NullBulk when there's none (e.g. LPOS or ZRANK) */
  protected final def numOrNull[A](o: Option[A])(f: A => Long): RESP = o.fold[RESP](NullBulk)(a => Num(f(a)))

  /** Compile errors are worded differently by refined/shapeless (Scala 2) and iron/dotty (Scala 3): on Scala 2 the exact `scala2` text is
    * checked, on Scala 3 only that compilation fails with an error containing `scala3`.
    */
  protected final def assertCompileErrors(obtained: String, scala2: String, scala3: String)(implicit loc: munit.Location): Unit =
    if (isScala3) assert(obtained.contains(scala3), s"expected a compile error containing [$scala3] but got:\n$obtained")
    else assertNoDiff(obtained, scala2)

  /** Like [[assertCompileErrors]], for errors whose exact Scala 2 text also varies between 2.12 and 2.13 */
  protected final def assertCompileErrorsContain(obtained: String, scala2: String, scala3: String)(implicit loc: munit.Location): Unit = {
    val expected = if (isScala3) scala3 else scala2
    assert(obtained.contains(expected), s"expected a compile error containing [$expected] but got:\n$obtained")
  }

  protected implicit final class EitherSyntax[A, B](private val eab: Either[A, B]) {
    def onRight[C](f: B => Boolean): Unit =
      eab.fold(err => fail(s"It Should be right but was left with $err"), b => assert(f(b)))

    def onRightAll[C](f: B => Unit): Unit =
      eab.fold(err => fail(s"It Should be right but was left with $err"), f)
  }
}
