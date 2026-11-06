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
    forAll { (ks: OneOrMoreKeys, t: NonNegInt, okv: Option[KV[(String, ValidDouble)]]) =>
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
    val scores                   = sortedsettypes.scorerange.closed(ValidDouble(1.0), ValidDouble(3.0))
    val names                    = sortedsettypes.lexrange.closed(Key("a"), Key("b"))
    val (k, o, c)                = (Key("k"), NonNegLong(1L), PosLong(2L))

    assertEquals(zrange[String](k, scores).encode, frame("ZRANGE", "k", "1.0", "3.0", "BYSCORE"))
    assertEquals(zrange[String](k, scores, o, c).encode, frame("ZRANGE", "k", "1.0", "3.0", "BYSCORE", "LIMIT", "1", "2"))
    assertEquals(zrange[String](k, names).encode, frame("ZRANGE", "k", "[a", "[b", "BYLEX"))
    assertEquals(zrange[String](k, names, o, c).encode, frame("ZRANGE", "k", "[a", "[b", "BYLEX", "LIMIT", "1", "2"))
    assertEquals(zrangewithscores[String](k, scores).encode, frame("ZRANGE", "k", "1.0", "3.0", "BYSCORE", "WITHSCORES"))
    assertEquals(
      zrangewithscores[String](k, scores, o, c).encode,
      frame("ZRANGE", "k", "1.0", "3.0", "BYSCORE", "LIMIT", "1", "2", "WITHSCORES")
    )
    assertEquals(zrevrange[String](k, scores).encode, frame("ZRANGE", "k", "3.0", "1.0", "BYSCORE", "REV"))
    assertEquals(zrevrange[String](k, scores, o, c).encode, frame("ZRANGE", "k", "3.0", "1.0", "BYSCORE", "REV", "LIMIT", "1", "2"))
    assertEquals(zrevrange[String](k, names).encode, frame("ZRANGE", "k", "[b", "[a", "BYLEX", "REV"))
    assertEquals(zrevrange[String](k, names, o, c).encode, frame("ZRANGE", "k", "[b", "[a", "BYLEX", "REV", "LIMIT", "1", "2"))
    assertEquals(zrevrangewithscores[String](k, scores).encode, frame("ZRANGE", "k", "3.0", "1.0", "BYSCORE", "REV", "WITHSCORES"))
    assertEquals(
      zrevrangewithscores[String](k, scores, o, c).encode,
      frame("ZRANGE", "k", "3.0", "1.0", "BYSCORE", "REV", "LIMIT", "1", "2", "WITHSCORES")
    )
    assertEquals(zrange[String](k, scores).decode(Arr(Bulk("a"))), Seq("a"))
    assertEquals(zrangewithscores[String](k, scores).decode(Arr(Bulk("a"), Bulk("1"))), Seq("a" -> 1.0))
  }
}
