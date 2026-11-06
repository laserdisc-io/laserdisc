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

/** What the tests of the overloads taking each key, member and score on its own pass to them, and what those look like when sent */
trait ExtPArguments {
  protected final def frame(params: String*): RESP = Arr(params.toList.map(Bulk(_)))

  protected final val (k, d)                                         = (Key("k"), Key("d"))
  protected final val (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11) =
    (Key("k1"), Key("k2"), Key("k3"), Key("k4"), Key("k5"), Key("k6"), Key("k7"), Key("k8"), Key("k9"), Key("k10"), Key("k11"))
  protected final val (k12, k13, k14, k15, k16, k17, k18, k19, k20, k21, k22) =
    (Key("k12"), Key("k13"), Key("k14"), Key("k15"), Key("k16"), Key("k17"), Key("k18"), Key("k19"), Key("k20"), Key("k21"), Key("k22"))
  protected final val (m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11) =
    ("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11")
  protected final val (m12, m13, m14, m15, m16, m17, m18, m19, m20, m21, m22) =
    ("m12", "m13", "m14", "m15", "m16", "m17", "m18", "m19", "m20", "m21", "m22")
  protected final val (s1, s2, s3, s4, s5, s6, s7) =
    (ValidDouble(1.5), ValidDouble(2.5), ValidDouble(3.5), ValidDouble(4.5), ValidDouble(5.5), ValidDouble(6.5), ValidDouble(7.5))
  protected final val (s8, s9, s10, s11, s12, s13, s14) =
    (ValidDouble(8.5), ValidDouble(9.5), ValidDouble(10.5), ValidDouble(11.5), ValidDouble(12.5), ValidDouble(13.5), ValidDouble(14.5))
  protected final val (s15, s16, s17, s18, s19, s20, s21) =
    (ValidDouble(15.5), ValidDouble(16.5), ValidDouble(17.5), ValidDouble(18.5), ValidDouble(19.5), ValidDouble(20.5), ValidDouble(21.5))
}
