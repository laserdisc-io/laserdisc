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

final class SetExtPSpec extends BaseSpec with SetP with ExtPArguments {
  //format: off
  test("The Set extended protocol using sadd sends a list of members passed as arguments") {
    val members  = (2 to 30).map(i => s"m$i")
    val expected = List("SADD", "k", "m1") ++ members
    assertEquals(sadd(k, m1, members*).encode, frame(expected*))
  }
  test("The Set extended protocol using sadd sends in order any number of members") {
    assertEquals(sadd(k, m1).encode, frame("SADD", "k", "m1"))
    assertEquals(sadd(k, m1, m2).encode, frame("SADD", "k", "m1", "m2"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21"))
  }
  test("The Set extended protocol using sdiff sends in order any number of keys") {
    assertEquals(sdiff[String](k1, k2).encode, frame("SDIFF", "k1", "k2"))
    assertEquals(sdiff[String](k1, k2, k3).encode, frame("SDIFF", "k1", "k2", "k3"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The Set extended protocol using sdiffstore sends in order from 2 to 21 keys") {
    assertEquals(sdiffstore(k1, k2, d).encode, frame("SDIFFSTORE", "d", "k1", "k2"))
    assertEquals(sdiffstore(k1, k2, k3, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3"))
    assertEquals(sdiffstore(k1, k2, k3, k4, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(sdiffstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, d).encode, frame("SDIFFSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
  }
  test("The Set extended protocol using sinter sends in order any number of keys") {
    assertEquals(sinter[String](k1, k2).encode, frame("SINTER", "k1", "k2"))
    assertEquals(sinter[String](k1, k2, k3).encode, frame("SINTER", "k1", "k2", "k3"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The Set extended protocol using sinterstore sends in order from 2 to 21 keys") {
    assertEquals(sinterstore(k1, k2, d).encode, frame("SINTERSTORE", "d", "k1", "k2"))
    assertEquals(sinterstore(k1, k2, k3, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3"))
    assertEquals(sinterstore(k1, k2, k3, k4, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(sinterstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, d).encode, frame("SINTERSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
  }
  test("The Set extended protocol using srem sends in order any number of members") {
    assertEquals(srem(k, m1).encode, frame("SREM", "k", "m1"))
    assertEquals(srem(k, m1, m2).encode, frame("SREM", "k", "m1", "m2"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21"))
  }
  test("The Set extended protocol using sunion sends in order any number of keys") {
    assertEquals(sunion[String](k1, k2).encode, frame("SUNION", "k1", "k2"))
    assertEquals(sunion[String](k1, k2, k3).encode, frame("SUNION", "k1", "k2", "k3"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The Set extended protocol using sunionstore sends in order from 2 to 21 keys") {
    assertEquals(sunionstore(k1, k2, d).encode, frame("SUNIONSTORE", "d", "k1", "k2"))
    assertEquals(sunionstore(k1, k2, k3, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3"))
    assertEquals(sunionstore(k1, k2, k3, k4, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(sunionstore(k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, d).encode, frame("SUNIONSTORE", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
  }
}
