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
package protocol

import Protocol.Single

object ArrayP {

  /** The first or last index, or an index, for ARGREP's range */
  sealed trait Bound { def param: String }
  object Bound       {
    private final class Of(override final val param: String) extends Bound

    final val min: Bound             = new Of("-")
    final val max: Bound             = new Of("+")
    def at(index: NonNegLong): Bound = new Of(Show[NonNegLong].show(index))
  }

  /** REV, for ARLASTITEMS: the most recent first */
  sealed trait Rev
  object Rev extends Rev {
    implicit val revShow: Show[Rev] = Show.const("REV")
  }

  /** FULL, for ARINFO: also how the array is stored */
  sealed trait Full
  object Full extends Full {
    implicit val fullShow: Show[Full] = Show.const("FULL")
  }

  /** What ARGREP looks for: a value that's exactly this, contains this, or matches a glob pattern or a regular expression */
  sealed trait Predicate { def params: List[String] }
  object Predicate       {
    private final class Of(override final val params: List[String]) extends Predicate

    def exact[A: Show](value: A): Predicate    = new Of(List("EXACT", Show[A].show(value)))
    def matching[A: Show](value: A): Predicate = new Of(List("MATCH", Show[A].show(value)))
    def glob(pattern: GlobPattern): Predicate  = new Of(List("GLOB", pattern.value))
    def re(pattern: String): Predicate         = new Of(List("RE", pattern))
  }

  /** AND, OR, LIMIT and NOCASE, for ARGREP: match all the predicates or any of them, stop after some matches, and ignore case */
  final class GrepOptions private (combine: List[String], limit: List[String], noCase: List[String]) {
    def and: GrepOptions                     = new GrepOptions(List("AND"), limit, noCase)
    def or: GrepOptions                      = new GrepOptions(List("OR"), limit, noCase)
    def limit(matches: PosLong): GrepOptions = new GrepOptions(combine, List("LIMIT", Show[PosLong].show(matches)), noCase)
    def nocase: GrepOptions                  = new GrepOptions(combine, limit, List("NOCASE"))

    private[protocol] def params: List[String] = combine ++ limit ++ noCase
  }
  object GrepOptions {
    private final val none: GrepOptions = new GrepOptions(Nil, Nil, Nil)

    def and: GrepOptions                     = none.and
    def or: GrepOptions                      = none.or
    def limit(matches: PosLong): GrepOptions = none.limit(matches)
    def nocase: GrepOptions                  = none.nocase
  }

  /** What AROP works out, by the kind of reply: a number, the bits of whole numbers, or how many elements */
  object Op {
    final class Number private[ArrayP] (private[protocol] val param: String)
    final class Bits private[ArrayP] (private[protocol] val param: String)
    final class Count private[ArrayP] (private[protocol] val params: List[String])

    final val sum: Number                  = new Number("SUM")
    final val min: Number                  = new Number("MIN")
    final val max: Number                  = new Number("MAX")
    final val and: Bits                    = new Bits("AND")
    final val or: Bits                     = new Bits("OR")
    final val xor: Bits                    = new Bits("XOR")
    final val used: Count                  = new Count(List("USED"))
    def matching[A: Show](value: A): Count = new Count(List("MATCH", Show[A].show(value)))
  }

  /** ARINFO's reply: how many elements are set, the highest index plus one, the next index of ARINSERT, and how the array is stored */
  final case class Info(
      count: NonNegLong,
      length: NonNegLong,
      nextInsertIndex: NonNegLong,
      slices: NonNegLong,
      directorySize: NonNegLong,
      superDirectoryEntries: NonNegLong,
      sliceSize: NonNegLong
  )

  /** ARINFO FULL's reply: [[Info]], and how many of the slices are dense and sparse, with their averages */
  final case class FullInfo(
      info: Info,
      denseSlices: NonNegLong,
      sparseSlices: NonNegLong,
      averageDenseSize: Double,
      averageDenseFill: Double,
      averageSparseSize: Double
  )

  private final def info(f: Fields): Either[RESPDecErr, Info] =
    for {
      count    <- f.num[NonNegLong]("count")
      length   <- f.num[NonNegLong]("len")
      next     <- f.num[NonNegLong]("next-insert-index")
      slices   <- f.num[NonNegLong]("slices")
      dirSize  <- f.num[NonNegLong]("directory-size")
      superDir <- f.num[NonNegLong]("super-dir-entries")
      slice    <- f.num[NonNegLong]("slice-size")
    } yield Info(count, length, next, slices, dirSize, superDir, slice)

  private[protocol] final val infoRead: Read[Record, Info]         = Read.record("array info")(info)
  private[protocol] final val fullInfoRead: Read[Record, FullInfo] = Read.record("array info") { f =>
    for {
      info       <- info(f)
      dense      <- f.num[NonNegLong]("dense-slices")
      sparse     <- f.num[NonNegLong]("sparse-slices")
      size       <- f.bulk[Double]("avg-dense-size")
      fill       <- f.bulk[Double]("avg-dense-fill")
      sparseSize <- f.bulk[Double]("avg-sparse-size")
    } yield FullInfo(info, dense, sparse, size, fill, sparseSize)
  }
}

/** The arrays of Redis 8.8: values at whole number indexes, with gaps. Indexes above the largest `Long` aren't supported, though a server
  * takes them, and replies with one fail to read
  */
trait ArrayP extends Features {
  import ArrayP.{Bound, Full, FullInfo, GrepOptions, Info, Op, Predicate, Rev}

  object arraytypes {
    final type ArrayBound       = ArrayP.Bound
    final type ArrayFull        = ArrayP.Full
    final type ArrayFullInfo    = ArrayP.FullInfo
    final type ArrayGrepOptions = ArrayP.GrepOptions
    final type ArrayInfo        = ArrayP.Info
    final type ArrayPredicate   = ArrayP.Predicate
    final type ArrayRev         = ArrayP.Rev

    final val ArrayBound       = ArrayP.Bound
    final val ArrayFull        = ArrayP.Full
    final val ArrayFullInfo    = ArrayP.FullInfo
    final val ArrayGrepOptions = ArrayP.GrepOptions
    final val ArrayInfo        = ArrayP.Info
    final val ArrayOp          = ArrayP.Op
    final val ArrayPredicate   = ArrayP.Predicate
    final val ArrayRev         = ArrayP.Rev
  }

  override private[laserdisc] def supported: List[String] =
    super.supported ++ List(
      "ARCOUNT",
      "ARDEL",
      "ARDELRANGE",
      "ARGET",
      "ARGETRANGE",
      "ARGREP: AND EXACT GLOB LIMIT MATCH NOCASE OR RE WITHVALUES",
      "ARINFO: FULL",
      "ARINSERT",
      "ARLASTITEMS: REV",
      "ARLEN",
      "ARMGET",
      "ARMSET",
      "ARNEXT",
      "AROP: AND MATCH MAX MIN OR SUM USED XOR",
      "ARRING",
      "ARSCAN: LIMIT",
      "ARSEEK",
      "ARSET"
    )

  /** How many elements are set */
  final def arcount(key: Key): Single.Aux[NonNegLong] = Protocol("ARCOUNT", key).as[Num, NonNegLong]

  /** Deletes the elements at these indexes, with how many there were */
  final def ardel(key: Key, indexes: OneOrMore[NonNegLong]): Single.Aux[NonNegLong] =
    Protocol("ARDEL", key.value :: indexes.value.map(Show[NonNegLong].show)).as[Num, NonNegLong]

  /** Deletes the elements in these ranges, with how many there were */
  final def ardelrange(key: Key, ranges: OneOrMore[(NonNegLong, NonNegLong)]): Single.Aux[NonNegLong] =
    Protocol(
      "ARDELRANGE",
      key.value :: ranges.value.flatMap { case (start, end) => List(Show[NonNegLong].show(start), Show[NonNegLong].show(end)) }
    )
      .as[Num, NonNegLong]

  final def arget[A: Read[Bulk, _]](key: Key, index: NonNegLong): Single.Aux[Option[A]] =
    Protocol("ARGET", key *: index *: EmptyTuple).as[OrNullBulk[Bulk], Option[A]]

  /** The elements from `start` to `end`, backwards if `end` is lower, with None for those not set */
  final def argetrange[A: Read[Bulk, _]](key: Key, start: NonNegLong, end: NonNegLong): Single.Aux[Seq[Option[A]]] =
    Protocol("ARGETRANGE", key *: start *: end *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[A]]]

  private final def grepParams(key: Key, start: Bound, end: Bound, predicates: OneOrMore[Predicate]): List[String] =
    key.value :: start.param :: end.param :: predicates.value.flatMap(_.params)

  /** The indexes of the elements from `start` to `end` that match */
  final def argrep(key: Key, start: Bound, end: Bound, predicates: OneOrMore[Predicate]): Single.Aux[Seq[NonNegLong]] =
    Protocol("ARGREP", grepParams(key, start, end, predicates)).as[Arr.Of[Num], Seq[NonNegLong]]
  final def argrep(
      key: Key,
      start: Bound,
      end: Bound,
      predicates: OneOrMore[Predicate],
      options: GrepOptions
  ): Single.Aux[Seq[NonNegLong]] =
    Protocol("ARGREP", grepParams(key, start, end, predicates) ++ options.params).as[Arr.Of[Num], Seq[NonNegLong]]

  /** The indexes and values of the elements from `start` to `end` that match */
  final def argrepwithvalues[A: Read[Bulk, _]](
      key: Key,
      start: Bound,
      end: Bound,
      predicates: OneOrMore[Predicate]
  ): Single.Aux[Seq[(NonNegLong, A)]] =
    Protocol("ARGREP", grepParams(key, start, end, predicates) :+ "WITHVALUES")
      .as[Arr.Of[Arr.AsTuple[Num *: Bulk *: EmptyTuple]], Seq[(NonNegLong, A)]]
  final def argrepwithvalues[A: Read[Bulk, _]](
      key: Key,
      start: Bound,
      end: Bound,
      predicates: OneOrMore[Predicate],
      options: GrepOptions
  ): Single.Aux[Seq[(NonNegLong, A)]] =
    Protocol("ARGREP", grepParams(key, start, end, predicates) ++ options.params :+ "WITHVALUES")
      .as[Arr.Of[Arr.AsTuple[Num *: Bulk *: EmptyTuple]], Seq[(NonNegLong, A)]]

  /** Fails with an error when the key doesn't exist */
  final def arinfo(key: Key): Single.Aux[Info]                 = Protocol("ARINFO", key).using(RESPRead.instance(ArrayP.infoRead))
  final def arinfo(key: Key, full: Full): Single.Aux[FullInfo] =
    Protocol("ARINFO", key *: full *: EmptyTuple).using(RESPRead.instance(ArrayP.fullInfoRead))

  /** Sets the values one after the other from the insert position of `arseek`, with the index of the last */
  final def arinsert[A: Show](key: Key, values: OneOrMore[A]): Single.Aux[NonNegLong] =
    Protocol("ARINSERT", key *: values.value *: EmptyTuple).as[Num, NonNegLong]

  /** The latest elements inserted with `arinsert` or `arring`, oldest first */
  final def arlastitems[A: Read[Bulk, _]](key: Key, count: PosLong): Single.Aux[Seq[Option[A]]] =
    Protocol("ARLASTITEMS", key *: count *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[A]]]
  final def arlastitems[A: Read[Bulk, _]](key: Key, count: PosLong, rev: Rev): Single.Aux[Seq[Option[A]]] =
    Protocol("ARLASTITEMS", key *: count *: rev *: EmptyTuple).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[A]]]

  /** The highest index set plus one */
  final def arlen(key: Key): Single.Aux[NonNegLong] = Protocol("ARLEN", key).as[Num, NonNegLong]

  /** The elements at these indexes, with None for those not set */
  final def armget[A: Read[Bulk, _]](key: Key, indexes: OneOrMore[NonNegLong]): Single.Aux[Seq[Option[A]]] =
    Protocol("ARMGET", key.value :: indexes.value.map(Show[NonNegLong].show)).as[Arr.Of[OrNullBulk[Bulk]], Seq[Option[A]]]

  /** Sets the values at their indexes, with how many weren't set before */
  final def armset[A: Show](key: Key, values: OneOrMore[(NonNegLong, A)]): Single.Aux[NonNegLong] =
    Protocol("ARMSET", key.value :: values.value.flatMap { case (index, value) => List(Show[NonNegLong].show(index), Show[A].show(value)) })
      .as[Num, NonNegLong]

  /** The index the next `arinsert` will use */
  final def arnext(key: Key): Single.Aux[NonNegLong] = Protocol("ARNEXT", key).as[Num, NonNegLong]

  /** The sum, lowest or highest of the numbers from `start` to `end`, or None if there are none */
  final def arop(key: Key, start: NonNegLong, end: NonNegLong, op: Op.Number): Single.Aux[Option[Double]] =
    Protocol("AROP", key *: start *: end *: op.param *: EmptyTuple).as[OrNullBulk[Bulk], Option[Double]]

  /** The AND, OR or XOR of the whole numbers from `start` to `end`, or None if there are none */
  final def arop(key: Key, start: NonNegLong, end: NonNegLong, op: Op.Bits): Single.Aux[Option[Long]] =
    Protocol("AROP", key *: start *: end *: op.param *: EmptyTuple).as[OrNullBulk[Num], Option[Long]]

  /** How many elements from `start` to `end` are set, or have a value */
  final def arop(key: Key, start: NonNegLong, end: NonNegLong, op: Op.Count): Single.Aux[NonNegLong] =
    Protocol("AROP", key *: start *: end *: op.params *: EmptyTuple).as[Num, NonNegLong]

  /** Sets the values one after the other in a ring buffer of `size`, overwriting the oldest when full, with the index of the last */
  final def arring[A: Show](key: Key, size: PosLong, values: OneOrMore[A]): Single.Aux[NonNegLong] =
    Protocol("ARRING", key *: size *: values.value *: EmptyTuple).as[Num, NonNegLong]

  /** The elements set from `start` to `end`, backwards if `end` is lower, with their indexes */
  final def arscan[A: Read[Bulk, _]](key: Key, start: NonNegLong, end: NonNegLong): Single.Aux[Seq[(NonNegLong, A)]] =
    Protocol("ARSCAN", key *: start *: end *: EmptyTuple).as[Arr.Of[Arr.AsTuple[Num *: Bulk *: EmptyTuple]], Seq[(NonNegLong, A)]]
  final def arscan[A: Read[Bulk, _]](key: Key, start: NonNegLong, end: NonNegLong, limit: PosLong): Single.Aux[Seq[(NonNegLong, A)]] =
    Protocol("ARSCAN", key *: start *: end *: "LIMIT" *: limit *: EmptyTuple)
      .as[Arr.Of[Arr.AsTuple[Num *: Bulk *: EmptyTuple]], Seq[(NonNegLong, A)]]

  /** Sets where `arinsert` writes next, with false if the key doesn't exist */
  final def arseek(key: Key, index: NonNegLong): Single.Aux[Boolean] = Protocol("ARSEEK", key *: index *: EmptyTuple).as[Num, Boolean]

  /** Sets the values one after the other from `index`, with how many weren't set before */
  final def arset[A: Show](key: Key, index: NonNegLong, values: OneOrMore[A]): Single.Aux[NonNegLong] =
    Protocol("ARSET", key *: index *: values.value *: EmptyTuple).as[Num, NonNegLong]
}
