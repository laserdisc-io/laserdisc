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

final class BForeverPSpec extends BaseSpec with Forever8_10 with ExtPArguments {
  import ListP.{Amount, Order, Side}
  import SortedSetP.Pop
  import StreamP.{From, GroupFrom, NoAck, ReadLimits}

  private def sent(forever: Forever[Any]): RESP = forever.protocol.encode

  private val (a, b)  = (Key("a"), Key("b"))
  private val keys    = OneOrMoreKeys.unsafeFrom(List(a, b))
  private val streams = OneOrMore.unsafeFrom(List(a -> From.latest))
  private val group   = OneOrMore.unsafeFrom(List(a -> GroupFrom.newEntries))
  private val limits  = ReadLimits.maxCount(PosLong(2L))

  test("The forever blocking list commands send a wait of 0") {
    assertEquals(sent(blpop[String](keys)), frame("BLPOP", "a", "b", "0"))
    assertEquals(sent(brpop[String](keys)), frame("BRPOP", "a", "b", "0"))
    assertEquals(sent(brpoplpush[String](a, b)), frame("BRPOPLPUSH", "a", "b", "0"))
    assertEquals(sent(blmove[String](a, b, Side.left, Side.right)), frame("BLMOVE", "a", "b", "LEFT", "RIGHT", "0"))
    assertEquals(sent(blmpop[String](keys, Side.left)), frame("BLMPOP", "0", "2", "a", "b", "LEFT"))
    assertEquals(sent(blmpop[String](keys, Side.left, PosInt(3))), frame("BLMPOP", "0", "2", "a", "b", "LEFT", "COUNT", "3"))
    assertEquals(sent(blmovem[String](a, b, Side.left, Side.right)), frame("BLMOVEM", "a", "b", "LEFT", "RIGHT", "0"))
    assertEquals(
      sent(blmovem[String](a, b, Side.left, Side.right, Amount.upTo(PosInt(2)), Order.oneByOne)),
      frame("BLMOVEM", "a", "b", "LEFT", "RIGHT", "0", "COUNT", "2", "OBO")
    )
  }

  test("The forever blocking sorted set commands send a wait of 0") {
    assertEquals(sent(bzpopmax[String](keys)), frame("BZPOPMAX", "a", "b", "0"))
    assertEquals(sent(bzpopmin[String](keys)), frame("BZPOPMIN", "a", "b", "0"))
    assertEquals(sent(bzmpop[String](keys, Pop.min)), frame("BZMPOP", "0", "2", "a", "b", "MIN"))
    assertEquals(sent(bzmpop[String](keys, Pop.max, PosInt(3))), frame("BZMPOP", "0", "2", "a", "b", "MAX", "COUNT", "3"))
  }

  test("The forever blocking stream commands send BLOCK 0") {
    assertEquals(sent(xread[Int](streams)), frame("XREAD", "BLOCK", "0", "STREAMS", "a", "$"))
    assertEquals(sent(xread[Int](streams, PosLong(5L))), frame("XREAD", "COUNT", "5", "BLOCK", "0", "STREAMS", "a", "$"))
    assertEquals(sent(xread[Int](streams, limits)), frame("XREAD", "MAXCOUNT", "2", "BLOCK", "0", "STREAMS", "a", "$"))
    assertEquals(
      sent(xread[Int](streams, PosLong(5L), limits)),
      frame("XREAD", "COUNT", "5", "MAXCOUNT", "2", "BLOCK", "0", "STREAMS", "a", "$")
    )
    assertEquals(sent(xreadgroup[Int](a, b, group)), frame("XREADGROUP", "GROUP", "a", "b", "BLOCK", "0", "STREAMS", "a", ">"))
    assertEquals(
      sent(xreadgroup[Int](a, b, group, PosLong(5L), NoAck)),
      frame("XREADGROUP", "GROUP", "a", "b", "COUNT", "5", "BLOCK", "0", "NOACK", "STREAMS", "a", ">")
    )
    assertEquals(
      sent(xreadgroup[Int](a, b, group, PosLong(5L), limits, NoAck)),
      frame("XREADGROUP", "GROUP", "a", "b", "COUNT", "5", "MAXCOUNT", "2", "BLOCK", "0", "NOACK", "STREAMS", "a", ">")
    )
    assertEquals(
      sent(xreadgroupclaim[Int](a, b, keys, NonNegLong(50L))),
      frame("XREADGROUP", "GROUP", "a", "b", "BLOCK", "0", "CLAIM", "50", "STREAMS", "a", "b", ">", ">")
    )
    assertEquals(
      sent(xreadgroupclaim[Int](a, b, keys, NonNegLong(50L), PosLong(5L), limits, NoAck)),
      frame(
        "XREADGROUP",
        "GROUP",
        "a",
        "b",
        "COUNT",
        "5",
        "MAXCOUNT",
        "2",
        "BLOCK",
        "0",
        "CLAIM",
        "50",
        "NOACK",
        "STREAMS",
        "a",
        "b",
        ">",
        ">"
      )
    )
  }

  test("A group or consumer named BLOCK doesn't move where the forever stream commands send their wait") {
    val block = Key("BLOCK")
    assertEquals(
      sent(xreadgroup[Int](block, block, group)),
      frame("XREADGROUP", "GROUP", "BLOCK", "BLOCK", "BLOCK", "0", "STREAMS", "a", ">")
    )
  }

  test("The forever WAIT and WAITAOF send a timeout of 0") {
    assertEquals(sent(waitreplicas(PosInt(1))), frame("WAIT", "1", "0"))
    assertEquals(sent(waitaof(NonNegInt(1), NonNegInt(2))), frame("WAITAOF", "1", "2", "0"))
  }

  test("A forever command reads its reply as the command with a wait does") {
    assertEquals(blpop[String](keys).protocol.decode(Arr(Bulk("a"), Bulk("x"))), Some(KV(a, "x")))
    assertEquals(blpop[String](keys).protocol.decode(NilArr), None)
  }

  test("A command that doesn't wait can't be made to wait forever") {
    intercept[IllegalArgumentException](Blocking.forever(Protocol("GET", a).as[Bulk, String]))
  }

  //format: off
  test("The forever Blocking List protocol using blpop sends in order from 1 to 22 keys") {
    assertEquals(blpop[String](k1).protocol.encode, frame("BLPOP", "k1", "0"))
    assertEquals(blpop[String](k1, k2).protocol.encode, frame("BLPOP", "k1", "k2", "0"))
    assertEquals(blpop[String](k1, k2, k3).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "0"))
    assertEquals(blpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).protocol.encode, frame("BLPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22", "0"))
  }
  test("The forever Blocking List protocol using brpop sends in order from 1 to 22 keys") {
    assertEquals(brpop[String](k1).protocol.encode, frame("BRPOP", "k1", "0"))
    assertEquals(brpop[String](k1, k2).protocol.encode, frame("BRPOP", "k1", "k2", "0"))
    assertEquals(brpop[String](k1, k2, k3).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "0"))
    assertEquals(brpop[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).protocol.encode, frame("BRPOP", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22", "0"))
  }
  //format: on
}
