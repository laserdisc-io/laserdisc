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
  test("The Set extended protocol using sadd sends in order from 1 to 21 members") {
    assertEquals(sadd(k, m1).encode, frame("SADD", "k", "m1"))
    assertEquals(sadd(k, m1, m2).encode, frame("SADD", "k", "m1", "m2"))
    assertEquals(sadd(k, m1, m2, m3).encode, frame("SADD", "k", "m1", "m2", "m3"))
    assertEquals(sadd(k, m1, m2, m3, m4).encode, frame("SADD", "k", "m1", "m2", "m3", "m4"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20"))
    assertEquals(sadd(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21).encode, frame("SADD", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21"))
  }
  test("The Set extended protocol using sdiff sends in order from 2 to 22 keys") {
    assertEquals(sdiff[String](k1, k2).encode, frame("SDIFF", "k1", "k2"))
    assertEquals(sdiff[String](k1, k2, k3).encode, frame("SDIFF", "k1", "k2", "k3"))
    assertEquals(sdiff[String](k1, k2, k3, k4).encode, frame("SDIFF", "k1", "k2", "k3", "k4"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(sdiff[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("SDIFF", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
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
  test("The Set extended protocol using sinter sends in order from 2 to 22 keys") {
    assertEquals(sinter[String](k1, k2).encode, frame("SINTER", "k1", "k2"))
    assertEquals(sinter[String](k1, k2, k3).encode, frame("SINTER", "k1", "k2", "k3"))
    assertEquals(sinter[String](k1, k2, k3, k4).encode, frame("SINTER", "k1", "k2", "k3", "k4"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(sinter[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("SINTER", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
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
  test("The Set extended protocol using srem sends in order from 1 to 21 members") {
    assertEquals(srem(k, m1).encode, frame("SREM", "k", "m1"))
    assertEquals(srem(k, m1, m2).encode, frame("SREM", "k", "m1", "m2"))
    assertEquals(srem(k, m1, m2, m3).encode, frame("SREM", "k", "m1", "m2", "m3"))
    assertEquals(srem(k, m1, m2, m3, m4).encode, frame("SREM", "k", "m1", "m2", "m3", "m4"))
    assertEquals(srem(k, m1, m2, m3, m4, m5).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20"))
    assertEquals(srem(k, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20, m21).encode, frame("SREM", "k", "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21"))
  }
  test("The Set extended protocol using sunion sends in order from 2 to 22 keys") {
    assertEquals(sunion[String](k1, k2).encode, frame("SUNION", "k1", "k2"))
    assertEquals(sunion[String](k1, k2, k3).encode, frame("SUNION", "k1", "k2", "k3"))
    assertEquals(sunion[String](k1, k2, k3, k4).encode, frame("SUNION", "k1", "k2", "k3", "k4"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(sunion[String](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("SUNION", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
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
