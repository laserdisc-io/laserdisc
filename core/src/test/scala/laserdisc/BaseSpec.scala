package laserdisc

import eu.timepit.refined.W
import eu.timepit.refined.api._
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.scalacheck.reftype.arbitraryRefType
import eu.timepit.refined.scalacheck.{CollectionInstancesBinCompat1, NumericInstances, StringInstances}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._

import org.scalatest.{Assertion, EitherValues, Matchers, WordSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.Double.{NaN, MaxValue => DMax, MinValue => DMin}
import scala.Int.{MaxValue => IMax, MinValue => IMin}
import scala.Long.{MaxValue => LMax, MinValue => LMin}

abstract class BaseSpec
    extends WordSpec
    with Matchers
    with EitherValues
    with ScalaCheckPropertyChecks
    with CollectionInstancesBinCompat1
    with NumericInstances
    with StringInstances {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = 100,
      maxDiscardedFactor = 10.0,
      minSize = 0,
      sizeRange = 100,
      workers = 12
    )

  protected final type EmptyString = String Refined Equal[W.`""`.T]
  protected final val EmptyString: EmptyString = RefType.applyRefM[EmptyString]("")

  private[this] final val dashChar: Char     = 0x002D.toChar
  private[this] final val dashString: String = dashChar.toString
  private[this] final val dotString: String  = 0x002E.toChar.toString
  private[this] final val spaceChar: Char    = 0x0020.toChar

  private[this] final val byteRange: Gen[Int] = chooseNum(0, 255)
  private[this] final val dashGen: Gen[Char]  = const(dashChar)
  private[this] final val hexGen: Gen[Char]   = frequency(10 -> numChar, 6 -> choose(0x0061.toChar, 0x0066.toChar))
  private[this] final val spaceGen: Gen[Char] = const(spaceChar)
  protected final val utf8BMPCharGen: Gen[Char] = {
    val b01 = 24 -> choose(spaceChar, 0x007E.toChar) // 75% it's a 7-bit ASCII char
    val b02 = 1  -> choose(0x00A0.toChar, 0x085F.toChar) // 3.125% for all other cases
    val b03 = 1  -> choose(0x08A0.toChar, 0x1AAF.toChar)
    val b04 = 1  -> choose(0x1B00.toChar, 0x1C7F.toChar)
    val b05 = 1  -> choose(0x1CC0.toChar, 0x2FDF.toChar)
    val b06 = 1  -> choose(0x2FF0.toChar, 0xA9DF.toChar)
    val b07 = 1  -> choose(0xAA00.toChar, 0xAB2F.toChar)
    val b08 = 1  -> choose(0xABC0.toChar, 0xD7FF.toChar)
    val b09 = 1  -> choose(0xE000.toChar, 0xFFEF.toChar)

    frequency(b01, b02, b03, b04, b05, b06, b07, b08, b09)
  }
  private[this] final val noSpaceUtf8BMPCharGen: Gen[Char] = utf8BMPCharGen.suchThat(_ != spaceChar)

  private[this] final val globGen: Gen[String] = {
    val basicGlob = nonEmptyListOf(frequency(1 -> const('*'), 1 -> const('?'), 1 -> alphaNumChar)).map(_.mkString)
    Gen.oneOf(0, 1).flatMap(f => if (f == 0) basicGlob else basicGlob.map(g => s"[$g]"))
  }
  private[this] final val allNICsGen: Gen[String] = const(AllNICsEqWit.value)
  private[this] final val lbGen: Gen[String]      = const(LoopbackEqWit.value)
  private[this] final val rfc1123Gen: Gen[String] = {
    def dashAtBeginOrEnd(s: String) = s.startsWith(dashString) || s.endsWith(dashString)
    val piece                       = chooseNum(1, 62).flatMap(strOfNGen(_, 1 -> dashGen, 99 -> alphaNumChar))
    choose(1, 5).flatMap(listOfN(_, piece.retryUntil(!dashAtBeginOrEnd(_))).map(_.mkString(dotString)).retryUntil(_.size <= 255))
  }
  private[this] final val rfc1918Gen: Gen[String] = Gen.oneOf(ipv4Gen(10), chooseNum(15, 31).flatMap(ipv4Gen(172, _)), ipv4Gen(192, 168))
  private[this] final val rfc5737Gen: Gen[String] = Gen.oneOf(ipv4Gen(192, 0, 2), ipv4Gen(198, 51, 100), ipv4Gen(203, 0, 113))
  private[this] final val rfc3927Gen: Gen[String] = ipv4Gen(169, 254)
  private[this] final val rfc2544Gen: Gen[String] = chooseNum(18, 19).flatMap(ipv4Gen(198, _))

  private[this] final def ipv4Gen(hs: Int*): Gen[String]            = listOfN(4 - hs.size, byteRange).map(ts => (hs ++: ts).mkString(dotString))
  private[this] final def twoOrMore[A](ga: => Gen[A]): Gen[List[A]] = nonEmptyListOf(ga).suchThat(_.size > 1)
  private[this] final def zip[A, B](arbA: => Arbitrary[A], arbB: => Arbitrary[B]): Gen[(A, B)] =
    arbA.arbitrary.flatMap(a => arbB.arbitrary.map(a -> _))

  private[this] final def strGen(lc: Gen[List[Char]]): Gen[String]               = lc.map(_.mkString)
  private[this] final def strOfNGen(n: Int, fs: (Int, Gen[Char])*): Gen[String]  = listOfN(n, frequency(fs: _*)).map(_.mkString)
  private[this] final def strOfNSameFreqGen(n: Int, gs: Gen[Char]*): Gen[String] = strOfNGen(n, gs.map(1 -> _): _*)

  final val connectionNameIsValid: String => Boolean                          = Validate[String, ConnectionNameRef].isValid
  final val dbIndexIsValid: Int => Boolean                                    = Validate[Int, DbIndexRef].isValid
  final val geoHashIsValid: String => Boolean                                 = Validate[String, GeoHashRef].isValid
  final val globPatternIsValid: String => Boolean                             = Validate[String, GlobPatternRef].isValid
  final val hostIsValid: String => Boolean                                    = Validate[String, HostRef].isValid
  final val keyIsValid: String => Boolean                                     = Validate[String, KeyRef].isValid
  final val latitudeIsValid: Double => Boolean                                = Validate[Double, LatitudeRef].isValid
  final val longitudeIsValid: Double => Boolean                               = Validate[Double, LongitudeRef].isValid
  final val nodeIdIsValid: String => Boolean                                  = Validate[String, NodeIdRef].isValid
  final val nonNegDoubleIsValid: Double => Boolean                            = Validate[Double, NonNegDoubleRef].isValid
  final val nonNegIntIsValid: Int => Boolean                                  = Validate[Int, NonNegRef].isValid
  final val nonNegLongIsValid: Long => Boolean                                = Validate[Long, NonNegRef].isValid
  final val nonZeroDoubleIsValid: Double => Boolean                           = Validate[Double, NonZeroDoubleRef].isValid
  final val nonZeroIntIsValid: Int => Boolean                                 = Validate[Int, NonZeroIntRef].isValid
  final val nonZeroLongIsValid: Long => Boolean                               = Validate[Long, NonZeroLongRef].isValid
  final def oneOrMoreIsValid[A]: List[A] => Boolean                           = Validate[List[A], OneOrMoreRef].isValid
  final val oneOrMoreKeysIsValid: List[Key] => Boolean                        = Validate[List[Key], OneOrMoreRef].isValid
  final val portIsValid: Int => Boolean                                       = Validate[Int, PortRef].isValid
  final val rangeOffsetIsValid: Int => Boolean                                = Validate[Int, RangeOffsetRef].isValid
  final val slotIsValid: Int => Boolean                                       = Validate[Int, SlotRef].isValid
  final val stringLengthIsValid: Long => Boolean                              = Validate[Long, StringLengthRef].isValid
  final val twoOrMoreKeysIsValid: List[Key] => Boolean                        = Validate[List[Key], TwoOrMoreRef].isValid
  final val twoOrMoreWeightedKeysIsValid: List[(Key, ValidDouble)] => Boolean = Validate[List[(Key, ValidDouble)], TwoOrMoreRef].isValid
  final val validDoubleIsValid: Double => Boolean                             = Validate[Double, ValidDoubleRef].isValid

  final val connectionNameGen: Gen[String] = strGen(nonEmptyListOf(noSpaceUtf8BMPCharGen)) :| "connection name"
  final val dbIndexGen: Gen[Int]           = chooseNum(0, DbIndexMaxValueWit.value) :| "db index"
  final val directionGen: Gen[Direction]   = Gen.oneOf(Direction.asc, Direction.desc) :| "direction"
  final val geoHashGen: Gen[String]        = strOfNGen(11, 1 -> numChar, 9 -> alphaLowerChar) :| "geo hash"
  final val globPatternGen: Gen[String]    = nonEmptyListOf(globGen).map(_.mkString) :| "glob pattern"
  final val hostGen: Gen[Host] = {
    val hosts = 7 -> Gen
      .oneOf(allNICsGen, lbGen, rfc1123Gen, rfc1918Gen, rfc5737Gen, rfc3927Gen, rfc2544Gen)
      .filter(hostIsValid)
      .map(hs => Host.unsafeFrom(hs))
    val loopbackHost = 1 -> Gen.const(LoopbackHost)

    frequency(hosts, loopbackHost)
  } :| "host"
  final val hostOrEmptyGen: Gen[EmptyString | Host] =
    frequency(10 -> hostGen.map(Right.apply), 1 -> const(EmptyString).map(Left.apply)) :| "host or empty string"
  final val keyGen: Gen[Key]                  = strGen(nonEmptyListOf(utf8BMPCharGen)).filter(keyIsValid).map(Key.unsafeFrom) :| "key"
  final val latitudeGen: Gen[Double]          = chooseNum(LatitudeMinValueWit.value, LatitudeMaxValueWit.value) :| "latitude"
  final val longitudeGen: Gen[Double]         = chooseNum(LongitudeMinValueWit.value, LongitudeMaxValueWit.value) :| "longitude"
  final val nodeIdGen: Gen[String]            = strOfNSameFreqGen(40, hexGen) :| "node id"
  final val nonNegDoubleGen: Gen[Double]      = chooseNum(0.0d, DMax) :| "double >= 0.0D"
  final val nonNegIntGen: Gen[Int]            = chooseNum(0, IMax) :| "int >= 0"
  final val nonNegLongGen: Gen[Long]          = chooseNum(0L, LMax) :| "long >= 0L"
  final val nonZeroDoubleGen: Gen[Double]     = chooseNum(DMin, DMax).suchThat(d => d != 0.0d && d != NaN) :| "double != 0.0D and != NaN"
  final val nonZeroIntGen: Gen[Int]           = chooseNum(IMin, IMax).suchThat(_ != 0) :| "int != 0"
  final val nonZeroLongGen: Gen[Long]         = chooseNum(LMin, LMax).suchThat(_ != 0L) :| "long != 0L"
  final val portGen: Gen[Port]                = chooseNum(PortMinValueWit.value, PortMaxValueWit.value).map(Port.unsafeFrom) :| "port"
  final val rangeOffsetGen: Gen[Int]          = chooseNum(0, RangeOffsetMaxValueWit.value) :| "range offset"
  final val slotGen: Gen[Slot]                = chooseNum(0, SlotMaxValueWit.value).map(Slot.unsafeFrom) :| "slot"
  final val stringLengthGen: Gen[Long]        = chooseNum(0L, StringLengthMaxValueWit.value) :| "string length"
  final val stringsWithSpacesGen: Gen[String] = strGen(listOf(frequency(1 -> spaceGen, 10 -> noSpaceUtf8BMPCharGen))) :| "string w/ spaces"
  final val validDoubleGen: Gen[Double]       = chooseNum(DMin, DMax).suchThat(_ != NaN) :| "double != NaN"

  implicit final val connectionNameArb: Arbitrary[ConnectionName] = arbitraryRefType(connectionNameGen)
  implicit final val directionArb: Arbitrary[Direction]           = Arbitrary(directionGen)
  implicit final val geoHashArb: Arbitrary[GeoHash]               = arbitraryRefType(geoHashGen)
  implicit final val globPatternArb: Arbitrary[GlobPattern]       = arbitraryRefType(globPatternGen)
  implicit final val hostArb: Arbitrary[Host]                     = Arbitrary(hostGen)
  implicit final val indexArb: Arbitrary[Index]                   = arbitraryRefType(arbitrary[Long])
  implicit final val keyArb: Arbitrary[Key]                       = Arbitrary(keyGen)
  implicit final def kvArb[A](implicit A: Arbitrary[A]): Arbitrary[KV[A]] =
    Arbitrary(keyArb.arbitrary.flatMap(k => A.arbitrary.map(KV(k, _))))
  implicit final val nodeIdArb: Arbitrary[NodeId]               = arbitraryRefType(nodeIdGen)
  implicit final val nonNegDoubleArb: Arbitrary[NonNegDouble]   = arbitraryRefType(nonNegDoubleGen)
  implicit final val nonNegIntArb: Arbitrary[NonNegInt]         = arbitraryRefType(nonNegIntGen)
  implicit final val nonNegLongArb: Arbitrary[NonNegLong]       = arbitraryRefType(nonNegLongGen)
  implicit final val nonZeroDoubleArb: Arbitrary[NonZeroDouble] = arbitraryRefType(nonZeroDoubleGen)
  implicit final val nonZeroIntArb: Arbitrary[NonZeroInt]       = arbitraryRefType(nonZeroIntGen)
  implicit final val nonZeroLongArb: Arbitrary[NonZeroLong]     = arbitraryRefType(nonZeroLongGen)
  implicit final val scanKeyArb: Arbitrary[Scan[Key]] =
    Arbitrary(nonNegLongArb.arbitrary.flatMap(l => option(listOf(keyArb.arbitrary)).map(Scan(l, _))))
  implicit final val scanKVArb: Arbitrary[ScanKV] =
    Arbitrary(nonNegLongArb.arbitrary.flatMap(l => option(listOf(kvArb[String].arbitrary)).map(ScanKV(l, _))))
  implicit final val slotArb: Arbitrary[Slot]                                   = Arbitrary(slotGen)
  implicit final val twoOrMoreKeysArb: Arbitrary[TwoOrMoreKeys]                 = arbitraryRefType(twoOrMore(keyArb.arbitrary))
  implicit final val twoOrMoreWeightedKeysArb: Arbitrary[TwoOrMoreWeightedKeys] = arbitraryRefType(twoOrMore(zip(keyArb, validDoubleArb)))
  implicit final val validDoubleArb: Arbitrary[ValidDouble]                     = arbitraryRefType(validDoubleGen)

  final val boolToNum: Boolean => Num = b => Num(if (b) 1 else 0)

  protected[this] implicit final class EitherSyntax[A, B](private val eab: Either[A, B]) {
    def onRight[C](f: B => Assertion): Assertion =
      eab.fold(err => fail(s"It Should be right but was left with $err"), f)

    def onLeft[C](e: A => Assertion): Assertion =
      eab.fold(e, res => fail(s"It Should be left but was right with $res"))

    def onRightAll[C](f: B => Unit): Unit =
      eab.fold(err => fail(s"It Should be right but was left with $err"), f)
  }
}
