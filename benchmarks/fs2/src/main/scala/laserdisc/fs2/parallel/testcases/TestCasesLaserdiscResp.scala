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
package fs2
package parallel
package testcases

import _root_.fs2.Chunk
import cats.effect.kernel.Concurrent
import laserdisc.protocol.RESP

private[fs2] object TestCasesLaserdiscResp {
  final def apply[F[_]: Concurrent](ch: Pipe[F, RESP, RESP]): TestCasesLaserdiscResp[F] =
    new TestCasesLaserdiscResp[F](ch) {}
}

private[fs2] abstract class TestCasesLaserdiscResp[F[_]: Concurrent](ch: Pipe[F, RESP, RESP]) extends TestSendResp {
  final def case1 = longSend.through(ch).compile.toVector
  final def case2 = shortSend.through(ch).compile.toVector
}

private[fs2] trait TestSendResp extends TestCommandsResp {

  protected val longSend =
    Stream.chunk(
      Chunk(
        longResp1,
        longResp2,
        longResp3,
        longResp4,
        longResp5,
        longResp6,
        longResp7,
        longResp8,
        longResp9,
        longResp10,
        longResp11,
        longResp12,
        longResp13,
        longResp14,
        longResp15,
        longResp16,
        longResp17,
        longResp18,
        longResp19,
        longResp20,
        longResp21,
        longResp22,
        longResp23,
        longResp24,
        longResp25,
        longResp26,
        longResp27,
        longResp28,
        longResp29,
        longResp30,
        longResp31,
        longResp32,
        longResp33,
        longResp34,
        longResp35,
        longResp36,
        longResp37,
        longResp38,
        longResp39,
        longResp40,
        longResp41,
        longResp42,
        longResp43,
        longResp44,
        longResp45,
        longResp46,
        longResp47,
        longResp48
      )
    )

  protected val shortSend =
    Stream.chunk(
      Chunk(
        shortResp1,
        shortResp2,
        shortResp3,
        shortResp4,
        shortResp5,
        shortResp6,
        shortResp7,
        shortResp8,
        shortResp9,
        shortResp10,
        shortResp11,
        shortResp12,
        shortResp13,
        shortResp14,
        shortResp15,
        shortResp16,
        shortResp17,
        shortResp18,
        shortResp19,
        shortResp20,
        shortResp21,
        shortResp22,
        shortResp23,
        shortResp24,
        shortResp25,
        shortResp26,
        shortResp27,
        shortResp28,
        shortResp29,
        shortResp30,
        shortResp31,
        shortResp32,
        shortResp33,
        shortResp34,
        shortResp35,
        shortResp36,
        shortResp37,
        shortResp38,
        shortResp39,
        shortResp40,
        shortResp41,
        shortResp42,
        shortResp43,
        shortResp44,
        shortResp45,
        shortResp46,
        shortResp47,
        shortResp48
      )
    )
}
