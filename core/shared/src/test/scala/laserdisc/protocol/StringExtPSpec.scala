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

final class StringExtPSpec extends BaseSpec with StringP with ExtPArguments {
  private val bitwise = stringtypes.bitwise.xor

  //format: off
  test("The String extended protocol using bitop sends in order from 2 to 20 keys") {
    assertEquals(bitop(k1, bitwise, k2, d).encode, frame("BITOP", "XOR", "d", "k1", "k2"))
    assertEquals(bitop(k1, bitwise, k2, k3, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(bitop(k1, bitwise, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, d).encode, frame("BITOP", "XOR", "d", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
  }
  test("The String extended protocol using mget sends in order from 1 to 22 keys") {
    assertEquals(mget[Seq[Option[String]]](k1).encode, frame("MGET", "k1"))
    assertEquals(mget[Seq[Option[String]]](k1, k2).encode, frame("MGET", "k1", "k2"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3).encode, frame("MGET", "k1", "k2", "k3"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4).encode, frame("MGET", "k1", "k2", "k3", "k4"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21"))
    assertEquals(mget[Seq[Option[String]]](k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22).encode, frame("MGET", "k1", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10", "k11", "k12", "k13", "k14", "k15", "k16", "k17", "k18", "k19", "k20", "k21", "k22"))
  }
  test("The String extended protocol using mset sends in order from 1 to 22 keys") {
    assertEquals(mset(k1, m1).encode, frame("MSET", "k1", "m1"))
    assertEquals(mset(k1, m1, k2, m2).encode, frame("MSET", "k1", "m1", "k2", "m2"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19, k20, m20).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19", "k20", "m20"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19, k20, m20, k21, m21).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19", "k20", "m20", "k21", "m21"))
    assertEquals(mset(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19, k20, m20, k21, m21, k22, m22).encode, frame("MSET", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19", "k20", "m20", "k21", "m21", "k22", "m22"))
  }
  test("The String extended protocol using msetnx sends in order from 1 to 22 keys") {
    assertEquals(msetnx(k1, m1).encode, frame("MSETNX", "k1", "m1"))
    assertEquals(msetnx(k1, m1, k2, m2).encode, frame("MSETNX", "k1", "m1", "k2", "m2"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19, k20, m20).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19", "k20", "m20"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19, k20, m20, k21, m21).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19", "k20", "m20", "k21", "m21"))
    assertEquals(msetnx(k1, m1, k2, m2, k3, m3, k4, m4, k5, m5, k6, m6, k7, m7, k8, m8, k9, m9, k10, m10, k11, m11, k12, m12, k13, m13, k14, m14, k15, m15, k16, m16, k17, m17, k18, m18, k19, m19, k20, m20, k21, m21, k22, m22).encode, frame("MSETNX", "k1", "m1", "k2", "m2", "k3", "m3", "k4", "m4", "k5", "m5", "k6", "m6", "k7", "m7", "k8", "m8", "k9", "m9", "k10", "m10", "k11", "m11", "k12", "m12", "k13", "m13", "k14", "m14", "k15", "m15", "k16", "m16", "k17", "m17", "k18", "m18", "k19", "m19", "k20", "m20", "k21", "m21", "k22", "m22"))
  }
}
