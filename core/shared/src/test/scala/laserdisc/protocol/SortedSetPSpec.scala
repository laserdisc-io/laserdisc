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

import org.scalacheck.Prop.forAll

final class SortedSetPSpec extends BaseSpec with SortedSetP with BSortedSetP {
  private def bulks(ss: String*): Arr                                          = Arr(ss.map(Bulk(_)).toList)
  private def scored(ps: List[(String, ValidDouble)]): List[GenBulk]           = ps.flatMap { case (m, s) => List(Bulk(m), Bulk(s)) }
  private def doubles(ps: List[(String, ValidDouble)]): List[(String, Double)] = ps.map { case (m, s) => m -> s.value }

  property("The SortedSet protocol using zaddincr sends INCR and reads the new score") {
    forAll { (k: Key, m: String, i: NonZeroDouble, s: ValidDouble) =>
      val protocol = zaddincr(k, m, i)
      assertEquals(protocol.encode, Arr(Bulk("ZADD"), Bulk(k), Bulk("INCR"), Bulk(i), Bulk(m)))
      assertEquals(protocol.decode(Bulk(s)), s.value)
    }
  }

  property("The SortedSet protocol using zaddincr with a flag reads None when nothing changed") {
    forAll { (k: Key, m: String, i: NonZeroDouble, os: Option[ValidDouble]) =>
      val protocol = zaddincr(k, sortedsettypes.flag.xxGt, m, i)
      assertEquals(protocol.encode, Arr(Bulk("ZADD"), Bulk(k), Bulk("XX"), Bulk("GT"), Bulk("INCR"), Bulk(i), Bulk(m)))
      assertEquals(protocol.decode(os.fold(NullBulk: GenBulk)(Bulk(_))), os.map(_.value))
    }
  }

  property("The SortedSet protocol using zmscore reads a score or None for each member") {
    forAll { (k: Key, ms: OneOrMore[String], oss: List[Option[ValidDouble]]) =>
      val protocol = zmscore(k, ms)
      assertEquals(protocol.encode, Arr(Bulk("ZMSCORE") :: Bulk(k) :: ms.value.map(Bulk(_))))
      assertEquals(protocol.decode(Arr(oss.map(_.fold(NullBulk: GenBulk)(Bulk(_))))), oss.map(_.map(_.value)))
    }
  }

  property("The SortedSet protocol using zpopmin reads the member and its score, or None when the key is empty") {
    forAll { (k: Key, op: Option[(String, ValidDouble)]) =>
      val protocol = zpopmin[String](k)
      assertEquals(protocol.encode, Arr(Bulk("ZPOPMIN"), Bulk(k)))
      assertEquals(protocol.decode(Arr(scored(op.toList))), op.map { case (m, s) => m -> s.value })
    }
  }

  property("The SortedSet protocol using zpopmax with a count reads members and scores") {
    forAll { (k: Key, c: PosInt, ps: List[(String, ValidDouble)]) =>
      val protocol = zpopmax[String](k, c)
      assertEquals(protocol.encode, Arr(Bulk("ZPOPMAX"), Bulk(k), Bulk(c)))
      assertEquals(protocol.decode(Arr(scored(ps))), doubles(ps))
    }
  }

  property("The SortedSet protocol using zrangestore sends BYSCORE, REV and LIMIT") {
    forAll { (s: Key, d: Key, min: ValidDouble, max: ValidDouble, o: NonNegLong, c: PosLong, n: NonNegInt) =>
      val range    = sortedsettypes.scorerange.openClosed(min, max)
      val protocol = zrevrangestore(s, d, range, o, c)
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("ZRANGESTORE"),
          Bulk(d),
          Bulk(s),
          Bulk(range.max),
          Bulk(range.min),
          Bulk("BYSCORE"),
          Bulk("REV"),
          Bulk("LIMIT"),
          Bulk(o),
          Bulk(c)
        )
      )
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The SortedSet protocol using zinterweightedwithscores sends WEIGHTS, AGGREGATE and WITHSCORES") {
    forAll { (wks: TwoOrMoreWeightedKeys, ps: List[(String, ValidDouble)]) =>
      val protocol = zinterweightedwithscores[String](wks, sortedsettypes.aggregate.min)
      val (ks, ws) = wks.value.unzip
      assertEquals(
        protocol.encode,
        Arr(
          Bulk("ZINTER") :: Bulk(ks.size) :: ks.map(Bulk(_)) ::: Bulk("WEIGHTS") :: ws.map(Bulk(_)) :::
            List(Bulk("AGGREGATE"), Bulk("MIN"), Bulk("WITHSCORES"))
        )
      )
      assertEquals(protocol.decode(Arr(scored(ps))), doubles(ps))
    }
  }

  property("The SortedSet protocol using zunionstore sends AGGREGATE COUNT") {
    forAll { (ks: TwoOrMoreKeys, d: Key, n: NonNegInt) =>
      val protocol = zunionstore(ks, d, sortedsettypes.aggregate.count)
      assertEquals(
        protocol.encode,
        Arr(Bulk("ZUNIONSTORE") :: Bulk(d) :: Bulk(ks.value.size) :: ks.value.map(Bulk(_)) ::: List(Bulk("AGGREGATE"), Bulk("COUNT")))
      )
      assertEquals(protocol.decode(Num(n.value.toLong)), n)
    }
  }

  property("The SortedSet protocol using zmpop reads the key with its members and scores, or None") {
    forAll { (ks: OneOrMoreKeys, c: PosInt, okv: Option[KV[List[(String, ValidDouble)]]]) =>
      val protocol = zmpop[String](ks, sortedsettypes.pop.max, c)
      assertEquals(
        protocol.encode,
        Arr(Bulk("ZMPOP") :: Bulk(ks.value.size) :: ks.value.map(Bulk(_)) ::: List(Bulk("MAX"), Bulk("COUNT"), Bulk(c)))
      )
      val reply = okv.fold(NilArr: GenArr) { case KV(k, ps) =>
        Arr(Bulk(k), Arr(ps.map { case (m, s) => Arr(Bulk(m), Bulk(s)) }))
      }
      assertEquals(protocol.decode(reply), okv.map { case KV(k, ps) => KV(k, doubles(ps)) })
    }
  }

  property("The SortedSet protocol using bzpopmin reads the key, member and score, or None") {
    forAll { (ks: OneOrMoreKeys, t: BlockingSeconds, okv: Option[KV[(String, ValidDouble)]]) =>
      val protocol = bzpopmin[String](ks, t)
      assertEquals(protocol.encode, Arr(Bulk("BZPOPMIN") :: ks.value.map(Bulk(_)) ::: List(Bulk(t))))
      val reply = okv.fold(NilArr: GenArr) { case KV(k, (m, s)) => Arr(Bulk(k), Bulk(m), Bulk(s)) }
      assertEquals(protocol.decode(reply), okv.map { case KV(k, (m, s)) => KV(k, m -> s.value) })
    }
  }

  property("The SortedSet protocol using zrankwithscore reads the rank and score, or None") {
    forAll { (k: Key, m: String, o: Option[(NonNegInt, ValidDouble)]) =>
      val protocol = zrankwithscore(k, m)
      assertEquals(protocol.encode, Arr(Bulk("ZRANK"), Bulk(k), Bulk(m), Bulk("WITHSCORE")))
      val reply = o.fold(NilArr: GenArr) { case (r, s) => Arr(Num(r.value.toLong), Bulk(s)) }
      assertEquals(protocol.decode(reply), o.map { case (r, s) => r -> s.value })
    }
  }

  test("The SortedSet protocol using zrange and zrevrange sends a range of scores or of names, with REV, LIMIT and WITHSCORES") {
    def frame(ps: String*): RESP = Arr(ps.map(Bulk(_)).toList)
    val scores                   = sortedsettypes.scorerange.closed(ValidDouble(1.5), ValidDouble(3.5))
    val names                    = sortedsettypes.lexrange.closed(Key("a"), Key("b"))
    val (k, o, c)                = (Key("k"), NonNegLong(1L), PosLong(2L))

    assertEquals(zrange[String](k, scores).encode, frame("ZRANGE", "k", "1.5", "3.5", "BYSCORE"))
    assertEquals(zrange[String](k, scores, o, c).encode, frame("ZRANGE", "k", "1.5", "3.5", "BYSCORE", "LIMIT", "1", "2"))
    assertEquals(zrange[String](k, names).encode, frame("ZRANGE", "k", "[a", "[b", "BYLEX"))
    assertEquals(zrange[String](k, names, o, c).encode, frame("ZRANGE", "k", "[a", "[b", "BYLEX", "LIMIT", "1", "2"))
    assertEquals(zrangewithscores[String](k, scores).encode, frame("ZRANGE", "k", "1.5", "3.5", "BYSCORE", "WITHSCORES"))
    assertEquals(
      zrangewithscores[String](k, scores, o, c).encode,
      frame("ZRANGE", "k", "1.5", "3.5", "BYSCORE", "LIMIT", "1", "2", "WITHSCORES")
    )
    assertEquals(zrevrange[String](k, scores).encode, frame("ZRANGE", "k", "3.5", "1.5", "BYSCORE", "REV"))
    assertEquals(zrevrange[String](k, scores, o, c).encode, frame("ZRANGE", "k", "3.5", "1.5", "BYSCORE", "REV", "LIMIT", "1", "2"))
    assertEquals(zrevrange[String](k, names).encode, frame("ZRANGE", "k", "[b", "[a", "BYLEX", "REV"))
    assertEquals(zrevrange[String](k, names, o, c).encode, frame("ZRANGE", "k", "[b", "[a", "BYLEX", "REV", "LIMIT", "1", "2"))
    assertEquals(zrevrangewithscores[String](k, scores).encode, frame("ZRANGE", "k", "3.5", "1.5", "BYSCORE", "REV", "WITHSCORES"))
    assertEquals(
      zrevrangewithscores[String](k, scores, o, c).encode,
      frame("ZRANGE", "k", "3.5", "1.5", "BYSCORE", "REV", "LIMIT", "1", "2", "WITHSCORES")
    )
    assertEquals(zrange[String](k, scores).decode(Arr(Bulk("a"))), Seq("a"))
    assertEquals(zrangewithscores[String](k, scores).decode(Arr(Bulk("a"), Bulk("1"))), Seq("a" -> 1.0))
  }

  property("The SortedSet protocol using zadd sends the score before each member, and any flag first") {
    forAll { (k: Key, ps: OneOrMore[(String, ValidDouble)], n: NonNegInt) =>
      val pairs = ps.value.flatMap { case (m, s) => List(Bulk(s), Bulk(m)) }
      assertEquals(zadd(k, ps).encode, Arr(Bulk("ZADD") :: Bulk(k) :: pairs))
      assertEquals(zadd(k, sortedsettypes.flag.nx, ps).encode, Arr(Bulk("ZADD") :: Bulk(k) :: Bulk("NX") :: pairs))
      assertEquals(zadd(k, ps).decode(Num(n.value.toLong)), n)
      assertEquals(zaddch(k, ps).encode, Arr(Bulk("ZADD") :: Bulk(k) :: Bulk("CH") :: pairs))
      assertEquals(
        zaddch(k, sortedsettypes.flag.xxLt, ps).encode,
        Arr(Bulk("ZADD") :: Bulk(k) :: Bulk("XX") :: Bulk("LT") :: Bulk("CH") :: pairs)
      )
      assertEquals(zaddch(k, ps).decode(Num(n.value.toLong)), n)
    }
  }

  property("The SortedSet protocol using zaddchincr sends CH then INCR, and with a flag reads None when nothing changed") {
    forAll { (k: Key, m: String, i: NonZeroDouble, os: Option[ValidDouble], s: ValidDouble) =>
      assertEquals(zaddchincr(k, m, i).encode, Arr(Bulk("ZADD"), Bulk(k), Bulk("CH"), Bulk("INCR"), Bulk(i), Bulk(m)))
      assertEquals(zaddchincr(k, m, i).decode(Bulk(s)), s.value)
      val flagged = zaddchincr(k, sortedsettypes.flag.nx, m, i)
      assertEquals(flagged.encode, Arr(Bulk("ZADD"), Bulk(k), Bulk("NX"), Bulk("CH"), Bulk("INCR"), Bulk(i), Bulk(m)))
      assertEquals(flagged.decode(os.fold(NullBulk: GenBulk)(Bulk(_))), os.map(_.value))
    }
  }

  property("The SortedSet protocol using zcard reads None for an empty sorted set") {
    forAll { (k: Key, n: PosInt) =>
      assertEquals(zcard(k).encode, Arr(Bulk("ZCARD"), Bulk(k)))
      assertEquals(zcard(k).decode(Num(n.value.toLong)), Some(n))
      assertEquals(zcard(k).decode(Num(0L)), None)
    }
  }

  property("The SortedSet protocol using zcount and zlexcount send the two ends of the range") {
    forAll { (k: Key, a: ValidDouble, b: ValidDouble, x: Key, y: Key, n: NonNegInt) =>
      val scores = sortedsettypes.scorerange.open(a, b)
      val names  = sortedsettypes.lexrange.closedOpen(x, y)
      assertEquals(zcount(k, scores).encode, Arr(Bulk("ZCOUNT"), Bulk(k), Bulk(scores.min), Bulk(scores.max)))
      assertEquals(zcount(k, scores).decode(Num(n.value.toLong)), n)
      assertEquals(zlexcount(k, names).encode, Arr(Bulk("ZLEXCOUNT"), Bulk(k), Bulk(names.min), Bulk(names.max)))
      assertEquals(zlexcount(k, names).decode(Num(n.value.toLong)), n)
    }
  }

  test("The SortedSet protocol writes the ends of each kind of range") {
    import sortedsettypes.{lexrange, scorerange}
    val (a, b)                         = (ValidDouble(1.5), ValidDouble(2.5))
    val (x, y)                         = (Key("x"), Key("y"))
    def ends(min: String, max: String) = min -> max

    assertEquals(ends(scorerange.open(a, b).min, scorerange.open(a, b).max), "(1.5"             -> "(2.5")
    assertEquals(ends(scorerange.openClosed(a, b).min, scorerange.openClosed(a, b).max), "(1.5" -> "2.5")
    assertEquals(ends(scorerange.closedOpen(a, b).min, scorerange.closedOpen(a, b).max), "1.5"  -> "(2.5")
    assertEquals(ends(scorerange.closed(a, b).min, scorerange.closed(a, b).max), "1.5"          -> "2.5")
    assertEquals(ends(lexrange.open(x, y).min, lexrange.open(x, y).max), "(x"                   -> "(y")
    assertEquals(ends(lexrange.openClosed(x, y).min, lexrange.openClosed(x, y).max), "(x"       -> "[y")
    assertEquals(ends(lexrange.closedOpen(x, y).min, lexrange.closedOpen(x, y).max), "[x"       -> "(y")
    assertEquals(ends(lexrange.closed(x, y).min, lexrange.closed(x, y).max), "[x"               -> "[y")
    assertEquals(ends(lexrange.openInf(x).min, lexrange.openInf(x).max), "(x"                   -> "+")
    assertEquals(ends(lexrange.closedInf(x).min, lexrange.closedInf(x).max), "[x"               -> "+")
    assertEquals(ends(lexrange.infOpen(y).min, lexrange.infOpen(y).max), "-"                    -> "(y")
    assertEquals(ends(lexrange.infClosed(y).min, lexrange.infClosed(y).max), "-"                -> "[y")
  }

  property("The SortedSet protocol using zincrby sends the increment before the member and reads the new score") {
    forAll { (k: Key, m: String, i: ValidDouble, s: ValidDouble) =>
      assertEquals(zincrby(k, m, i).encode, Arr(Bulk("ZINCRBY"), Bulk(k), Bulk(i), Bulk(m)))
      assertEquals(zincrby(k, m, i).decode(Bulk(s)), s.value)
    }
  }

  property("The SortedSet protocol using zrank and zrevrank reads the rank or None") {
    forAll { (k: Key, m: Key, r: Option[NonNegInt]) =>
      val reply = r.fold(NullBulk: RESP)(n => Num(n.value.toLong))
      assertEquals(zrank(k, m).encode, Arr(Bulk("ZRANK"), Bulk(k), Bulk(m)))
      assertEquals(zrank(k, m).decode(reply), r)
      assertEquals(zrevrank(k, m).encode, Arr(Bulk("ZREVRANK"), Bulk(k), Bulk(m)))
      assertEquals(zrevrank(k, m).decode(reply), r)
    }
  }

  property("The SortedSet protocol using zrem sends the members and reads how many it removed") {
    forAll { (k: Key, ms: OneOrMore[String], n: NonNegInt) =>
      assertEquals(zrem(k, ms).encode, Arr(Bulk("ZREM") :: Bulk(k) :: ms.value.map(Bulk(_))))
      assertEquals(zrem(k, ms).decode(Num(n.value.toLong)), n)
    }
  }

  property("The SortedSet protocol using zremrangebylex, zremrangebyrank and zremrangebyscore send the range") {
    forAll { (k: Key, x: Key, y: Key, a: ValidDouble, b: ValidDouble, s: Index, e: Index, n: NonNegInt) =>
      val names  = sortedsettypes.lexrange.closed(x, y)
      val scores = sortedsettypes.scorerange.closed(a, b)
      val reply  = Num(n.value.toLong)
      assertEquals(zremrangebylex(k, names).encode, Arr(Bulk("ZREMRANGEBYLEX"), Bulk(k), Bulk(names.min), Bulk(names.max)))
      assertEquals(zremrangebylex(k, names).decode(reply), n)
      assertEquals(zremrangebyrank(k, s, e).encode, Arr(Bulk("ZREMRANGEBYRANK"), Bulk(k), Bulk(s), Bulk(e)))
      assertEquals(zremrangebyrank(k, s, e).decode(reply), n)
      assertEquals(zremrangebyscore(k, scores).encode, Arr(Bulk("ZREMRANGEBYSCORE"), Bulk(k), Bulk(scores.min), Bulk(scores.max)))
      assertEquals(zremrangebyscore(k, scores).decode(reply), n)
    }
  }

  property("The SortedSet protocol using zrandmember reads a member or None, and zrandmembers several") {
    forAll { (k: Key, c: NonZeroInt, om: Option[String], ps: List[(String, ValidDouble)]) =>
      assertEquals(zrandmember[String](k).encode, Arr(Bulk("ZRANDMEMBER"), Bulk(k)))
      assertEquals(zrandmember[String](k).decode(om.fold(NullBulk: GenBulk)(Bulk(_))), om)
      assertEquals(zrandmembers[String](k, c).encode, Arr(Bulk("ZRANDMEMBER"), Bulk(k), Bulk(c)))
      assertEquals(zrandmembers[String](k, c).decode(Arr(ps.map { case (m, _) => Bulk(m) })), ps.map(_._1))
      assertEquals(zrandmemberswithscores[String](k, c).encode, Arr(Bulk("ZRANDMEMBER"), Bulk(k), Bulk(c), Bulk("WITHSCORES")))
      assertEquals(zrandmemberswithscores[String](k, c).decode(Arr(scored(ps))), doubles(ps))
    }
  }

  test("The SortedSet protocol using zrandmember reads the one member Dragonfly sends in an array") {
    assertEquals(zrandmember[String](Key("k")).decode(Arr(Bulk("a"))), Some("a"))
    assertEquals(zrandmember[String](Key("k")).decode(Arr(Nil)), None)
    assert(zrandmember[String](Key("k")).decode(Arr(Bulk("a"), Bulk("b"))).isLeft)
  }

  test("The SortedSet protocol using zrangebylex, zrangebyscore and their reverses sends each form") {
    def frame(ps: String*): RESP = Arr(ps.map(Bulk(_)).toList)
    val scores                   = sortedsettypes.scorerange.closed(ValidDouble(1.5), ValidDouble(3.5))
    val names                    = sortedsettypes.lexrange.closed(Key("a"), Key("b"))
    val (k, o, c)                = (Key("k"), NonNegLong(1L), PosLong(2L))
    val members                  = Arr(Bulk("a"), Bulk("b"))
    val withScores               = Arr(Bulk("a"), Bulk("1.5"), Bulk("b"), Bulk("2.5"))

    assertEquals(zrangebylex[String](k, names).encode, frame("ZRANGEBYLEX", "k", "[a", "[b"))
    assertEquals(zrangebylex[String](k, names, o, c).encode, frame("ZRANGEBYLEX", "k", "[a", "[b", "LIMIT", "1", "2"))
    assertEquals(zrangebylex[String](k, names).decode(members), Seq("a", "b"))
    assertEquals(zrevrangebylex[String](k, names).encode, frame("ZREVRANGEBYLEX", "k", "[b", "[a"))
    assertEquals(zrevrangebylex[String](k, names, o, c).encode, frame("ZREVRANGEBYLEX", "k", "[b", "[a", "LIMIT", "1", "2"))
    assertEquals(zrevrangebylex[String](k, names).decode(members), Seq("a", "b"))

    assertEquals(zrangebyscore[String](k, scores).encode, frame("ZRANGEBYSCORE", "k", "1.5", "3.5"))
    assertEquals(zrangebyscore[String](k, scores, o, c).encode, frame("ZRANGEBYSCORE", "k", "1.5", "3.5", "LIMIT", "1", "2"))
    assertEquals(zrangebyscore[String](k, scores).decode(members), Seq("a", "b"))
    assertEquals(zrevrangebyscore[String](k, scores).encode, frame("ZREVRANGEBYSCORE", "k", "3.5", "1.5"))
    assertEquals(zrevrangebyscore[String](k, scores, o, c).encode, frame("ZREVRANGEBYSCORE", "k", "3.5", "1.5", "LIMIT", "1", "2"))
    assertEquals(zrevrangebyscore[String](k, scores).decode(members), Seq("a", "b"))

    assertEquals(zrangebyscorewithscores[String](k, scores).encode, frame("ZRANGEBYSCORE", "k", "1.5", "3.5", "WITHSCORES"))
    assertEquals(
      zrangebyscorewithscores[String](k, scores, o, c).encode,
      frame("ZRANGEBYSCORE", "k", "1.5", "3.5", "WITHSCORES", "LIMIT", "1", "2")
    )
    assertEquals(zrangebyscorewithscores[String](k, scores).decode(withScores), Seq("a" -> 1.5, "b" -> 2.5))
    assertEquals(zrevrangebyscorewithscores[String](k, scores).encode, frame("ZREVRANGEBYSCORE", "k", "3.5", "1.5", "WITHSCORES"))
    assertEquals(
      zrevrangebyscorewithscores[String](k, scores, o, c).encode,
      frame("ZREVRANGEBYSCORE", "k", "3.5", "1.5", "WITHSCORES", "LIMIT", "1", "2")
    )
    assertEquals(zrevrangebyscorewithscores[String](k, scores).decode(withScores), Seq("a" -> 1.5, "b" -> 2.5))
  }

  test("The SortedSet protocol using zinter and zunion sends WEIGHTS, AGGREGATE and WITHSCORES in order") {
    def frame(ps: String*): RESP = Arr(ps.map(Bulk(_)).toList)
    val ks                       = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
    val w                        = TwoOrMoreWeightedKeys.unsafeFrom(List(Key("a") -> ValidDouble(1.5), Key("b") -> ValidDouble(2.5)))
    val max                      = sortedsettypes.aggregate.max
    val withScores               = Arr(Bulk("m"), Bulk("1.5"))

    assertEquals(zinter[String](ks).encode, frame("ZINTER", "2", "a", "b"))
    assertEquals(zinterweighted[String](w).encode, frame("ZINTER", "2", "a", "b", "WEIGHTS", "1.5", "2.5"))
    assertEquals(zinterweighted[String](w, max).encode, frame("ZINTER", "2", "a", "b", "WEIGHTS", "1.5", "2.5", "AGGREGATE", "MAX"))
    assertEquals(zinterwithscores[String](ks, max).encode, frame("ZINTER", "2", "a", "b", "AGGREGATE", "MAX", "WITHSCORES"))
    assertEquals(zinterweightedwithscores[String](w).encode, frame("ZINTER", "2", "a", "b", "WEIGHTS", "1.5", "2.5", "WITHSCORES"))
    assertEquals(zinterwithscores[String](ks, max).decode(withScores), Seq("m" -> 1.5))

    assertEquals(zunion[String](ks, max).encode, frame("ZUNION", "2", "a", "b", "AGGREGATE", "MAX"))
    assertEquals(zunionweighted[String](w).encode, frame("ZUNION", "2", "a", "b", "WEIGHTS", "1.5", "2.5"))
    assertEquals(zunionweighted[String](w, max).encode, frame("ZUNION", "2", "a", "b", "WEIGHTS", "1.5", "2.5", "AGGREGATE", "MAX"))
    assertEquals(zunionwithscores[String](ks).encode, frame("ZUNION", "2", "a", "b", "WITHSCORES"))
    assertEquals(zunionwithscores[String](ks, max).encode, frame("ZUNION", "2", "a", "b", "AGGREGATE", "MAX", "WITHSCORES"))
    assertEquals(
      zunionweightedwithscores[String](w, max).encode,
      frame("ZUNION", "2", "a", "b", "WEIGHTS", "1.5", "2.5", "AGGREGATE", "MAX", "WITHSCORES")
    )
    assertEquals(zunionwithscores[String](ks).decode(withScores), Seq("m" -> 1.5))
  }

  test("The SortedSet protocol using zinterstore and zunionstore sends the destination first, then WEIGHTS and AGGREGATE") {
    def frame(ps: String*): RESP = Arr(ps.map(Bulk(_)).toList)
    val (d, reply)               = (Key("d"), Num(3L))
    val ks                       = TwoOrMoreKeys.unsafeFrom(List(Key("a"), Key("b")))
    val w                        = TwoOrMoreWeightedKeys.unsafeFrom(List(Key("a") -> ValidDouble(1.5), Key("b") -> ValidDouble(2.5)))
    val sum                      = sortedsettypes.aggregate.sum

    assertEquals(zinterstore(ks, d).encode, frame("ZINTERSTORE", "d", "2", "a", "b"))
    assertEquals(zinterstore(ks, d, sum).encode, frame("ZINTERSTORE", "d", "2", "a", "b", "AGGREGATE", "SUM"))
    assertEquals(zinterstoreweighted(w, d).encode, frame("ZINTERSTORE", "d", "2", "a", "b", "WEIGHTS", "1.5", "2.5"))
    assertEquals(
      zinterstoreweighted(w, d, sum).encode,
      frame("ZINTERSTORE", "d", "2", "a", "b", "WEIGHTS", "1.5", "2.5", "AGGREGATE", "SUM")
    )
    assertEquals(zinterstore(ks, d).decode(reply), NonNegInt(3))

    assertEquals(zunionstore(ks, d).encode, frame("ZUNIONSTORE", "d", "2", "a", "b"))
    assertEquals(zunionstore(ks, d, sum).encode, frame("ZUNIONSTORE", "d", "2", "a", "b", "AGGREGATE", "SUM"))
    assertEquals(zunionstoreweighted(w, d).encode, frame("ZUNIONSTORE", "d", "2", "a", "b", "WEIGHTS", "1.5", "2.5"))
    assertEquals(
      zunionstoreweighted(w, d, sum).encode,
      frame("ZUNIONSTORE", "d", "2", "a", "b", "WEIGHTS", "1.5", "2.5", "AGGREGATE", "SUM")
    )
    assertEquals(zunionstore(ks, d).decode(reply), NonNegInt(3))
  }

  test("The SortedSet protocol using zscan sends each of its options, and reads members with their scores") {
    val (k, c, g, n) = (Key("k"), NonNegLong(5L), GlobPattern("a*"), PosInt(2))
    val reply        = Arr(Bulk("0"), Arr(Bulk("a"), Bulk("1.5")))
    val scan         = Scan(NonNegLong(0L), Some(Seq("a" -> 1.5d)))

    assertEquals(zscan[String](k, c).encode, bulks("ZSCAN", "k", "5"))
    assertEquals(zscan[String](k, c, g).encode, bulks("ZSCAN", "k", "5", "MATCH", "a*"))
    assertEquals(zscan[String](k, c, n).encode, bulks("ZSCAN", "k", "5", "COUNT", "2"))
    assertEquals(zscan[String](k, c, g, n).encode, bulks("ZSCAN", "k", "5", "MATCH", "a*", "COUNT", "2"))
    assertEquals(zscan[String](k, c, n).decode(reply), scan)
    assertEquals(zscan[String](k, c, g, n).decode(reply), scan)
  }

  test("The SortedSet protocol using zscannoscores sends each of its options, and reads only members") {
    val (k, c, g, n) = (Key("k"), NonNegLong(5L), GlobPattern("a*"), PosInt(2))
    val reply        = Arr(Bulk("0"), Arr(Bulk("a")))
    val scan         = Scan(NonNegLong(0L), Some(Seq("a")))

    assertEquals(zscannoscores[String](k, c).encode, bulks("ZSCAN", "k", "5", "NOSCORES"))
    assertEquals(zscannoscores[String](k, c, g).encode, bulks("ZSCAN", "k", "5", "MATCH", "a*", "NOSCORES"))
    assertEquals(zscannoscores[String](k, c, n).encode, bulks("ZSCAN", "k", "5", "COUNT", "2", "NOSCORES"))
    assertEquals(zscannoscores[String](k, c, g, n).encode, bulks("ZSCAN", "k", "5", "MATCH", "a*", "COUNT", "2", "NOSCORES"))
    assertEquals(zscannoscores[String](k, c).decode(reply), scan)
    assertEquals(zscannoscores[String](k, c, g).decode(reply), scan)
    assertEquals(zscannoscores[String](k, c, n).decode(reply), scan)
    assertEquals(zscannoscores[String](k, c, g, n).decode(reply), scan)
  }
}
