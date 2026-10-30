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

import cats.effect.kernel.{Outcome, Spawn}
import cats.effect.syntax.spawn.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.parallel.*
import cats.{FlatMap, Parallel}
import dev.profunktor.redis4cats.RedisCommands

private[fs2] object RedisForCatsTestCases {
  final def apply[F[_]: Spawn: Parallel](cl: RedisCommands[F, String, String]): RedisForCatsTestCases[F] =
    new RedisForCatsTestCases[F](cl) {}
}

private[fs2] sealed abstract class RedisForCatsTestCases[F[_]: Spawn: Parallel](cl: RedisCommands[F, String, String])
    extends TestCommandsRedisForCats(cl) {

  private final type Out[A] = Outcome[F, Throwable, A]

  final val case1: F[
    (
        Out[(Out[Long], Out[Unit], Out[Unit], Out[Unit], Out[List[String]], Out[Unit], Out[Long], Out[List[String]], Out[Unit], Out[Unit], Out[Long], Out[Unit], Out[Unit], Out[List[String]], Out[Unit], Out[Unit], Out[Unit], Out[Long], Out[List[String]], Out[Unit])],
        Out[(Out[Long], Out[Unit], Out[Unit], Out[Unit], Out[List[String]], Out[Unit], Out[Long], Out[List[String]], Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Long], Out[Unit], Out[List[String]], Out[Unit], Out[Unit], Out[Long], Out[List[String]], Out[Unit])],
        Out[(Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit])]
    )
  ] =
    for {
      r1  <- longSend1.start
      r2  <- longSend2.start
      r3  <- longSend3.start
      r4  <- longSend4.start
      r5  <- longSend5.start
      r6  <- longSend6.start
      r7  <- longSend7.start
      r8  <- longSend8.start
      r9  <- longSend9.start
      r10 <- longSend10.start
      r11 <- longSend11.start
      r12 <- longSend12.start
      r13 <- longSend13.start
      r14 <- longSend14.start
      r15 <- longSend15.start
      r16 <- longSend16.start
      r17 <- longSend17.start
      r18 <- longSend18.start
      r19 <- longSend19.start
      r20 <- longSend20.start
      r21 <- longSend21.start
      r22 <- longSend22.start
      r23 <- longSend23.start
      r24 <- longSend24.start
      r25 <- longSend25.start
      r26 <- longSend26.start
      r27 <- longSend27.start
      r28 <- longSend28.start
      r29 <- longSend29.start
      r30 <- longSend30.start
      r31 <- longSend31.start
      r32 <- longSend32.start
      r33 <- longSend33.start
      r34 <- longSend34.start
      r35 <- longSend35.start
      r36 <- longSend36.start
      r37 <- longSend37.start
      r38 <- longSend38.start
      r39 <- longSend39.start
      r40 <- longSend40.start
      r41 <- longSend41.start
      r42 <- longSend42.start
      r43 <- longSend43.start
      r44 <- longSend44.start
      r45 <- longSend45.start
      r46 <- longSend46.start
      r47 <- longSend47.start
      r48 <- longSend48.start
      jf1 <- (
        r1.join,
        r2.join,
        r3.join,
        r4.join,
        r5.join,
        r6.join,
        r7.join,
        r8.join,
        r9.join,
        r10.join,
        r11.join,
        r12.join,
        r13.join,
        r14.join,
        r15.join,
        r16.join,
        r17.join,
        r18.join,
        r19.join,
        r20.join
      ).parTupled.start
      jf2 <- (
        r21.join,
        r22.join,
        r23.join,
        r24.join,
        r25.join,
        r26.join,
        r27.join,
        r28.join,
        r29.join,
        r30.join,
        r31.join,
        r32.join,
        r33.join,
        r34.join,
        r35.join,
        r36.join,
        r37.join,
        r38.join,
        r39.join,
        r40.join
      ).parTupled.start
      jf3 <- (
        r41.join,
        r42.join,
        r43.join,
        r44.join,
        r45.join,
        r46.join,
        r47.join,
        r48.join
      ).parTupled.start
      j1 <- jf1.join
      j2 <- jf2.join
      j3 <- jf3.join
    } yield (j1, j2, j3)

  final val case2: F[
    (
        Out[(Out[Long], Out[Unit], Out[Unit], Out[Unit], Out[List[String]], Out[Unit], Out[Long], Out[List[String]], Out[Unit], Out[Unit], Out[Long], Out[Unit], Out[Unit], Out[List[String]], Out[Unit], Out[Unit], Out[Unit], Out[Long], Out[List[String]], Out[Unit])],
        Out[(Out[Long], Out[Unit], Out[Unit], Out[Unit], Out[List[String]], Out[Unit], Out[Long], Out[List[String]], Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Long], Out[Unit], Out[List[String]], Out[Unit], Out[Unit], Out[Long], Out[List[String]], Out[Unit])],
        Out[(Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit])]
    )
  ] =
    for {
      r1  <- shortSend1.start
      r2  <- shortSend2.start
      r3  <- shortSend3.start
      r4  <- shortSend4.start
      r5  <- shortSend5.start
      r6  <- shortSend6.start
      r7  <- shortSend7.start
      r8  <- shortSend8.start
      r9  <- shortSend9.start
      r10 <- shortSend10.start
      r11 <- shortSend11.start
      r12 <- shortSend12.start
      r13 <- shortSend13.start
      r14 <- shortSend14.start
      r15 <- shortSend15.start
      r16 <- shortSend16.start
      r17 <- shortSend17.start
      r18 <- shortSend18.start
      r19 <- shortSend19.start
      r20 <- shortSend20.start
      r21 <- shortSend21.start
      r22 <- shortSend22.start
      r23 <- shortSend23.start
      r24 <- shortSend24.start
      r25 <- shortSend25.start
      r26 <- shortSend26.start
      r27 <- shortSend27.start
      r28 <- shortSend28.start
      r29 <- shortSend29.start
      r30 <- shortSend30.start
      r31 <- shortSend31.start
      r32 <- shortSend32.start
      r33 <- shortSend33.start
      r34 <- shortSend34.start
      r35 <- shortSend35.start
      r36 <- shortSend36.start
      r37 <- shortSend37.start
      r38 <- shortSend38.start
      r39 <- shortSend39.start
      r40 <- shortSend40.start
      r41 <- shortSend41.start
      r42 <- shortSend42.start
      r43 <- shortSend43.start
      r44 <- shortSend44.start
      r45 <- shortSend45.start
      r46 <- shortSend46.start
      r47 <- shortSend47.start
      r48 <- shortSend48.start
      jf1 <- (
        r1.join,
        r2.join,
        r3.join,
        r4.join,
        r5.join,
        r6.join,
        r7.join,
        r8.join,
        r9.join,
        r10.join,
        r11.join,
        r12.join,
        r13.join,
        r14.join,
        r15.join,
        r16.join,
        r17.join,
        r18.join,
        r19.join,
        r20.join
      ).parTupled.start
      jf2 <- (
        r21.join,
        r22.join,
        r23.join,
        r24.join,
        r25.join,
        r26.join,
        r27.join,
        r28.join,
        r29.join,
        r30.join,
        r31.join,
        r32.join,
        r33.join,
        r34.join,
        r35.join,
        r36.join,
        r37.join,
        r38.join,
        r39.join,
        r40.join
      ).parTupled.start
      jf3 <- (
        r41.join,
        r42.join,
        r43.join,
        r44.join,
        r45.join,
        r46.join,
        r47.join,
        r48.join
      ).parTupled.start
      j1 <- jf1.join
      j2 <- jf2.join
      j3 <- jf3.join
    } yield (j1, j2, j3)

  final val case3: F[(Out[List[String]], Out[Unit], Out[Unit], Out[Unit], Out[List[String]], Out[Unit], Out[List[String]], Out[Unit], Out[Unit], Out[Unit])] =
    for {
      r1  <- longChain1.start
      r2  <- longChain2.start
      r3  <- longChain3.start
      r4  <- longChain4.start
      r5  <- longChain5.start
      r6  <- longChain6.start
      r7  <- longChain7.start
      r8  <- longChain8.start
      r9  <- longChain9.start
      r10 <- longChain10.start
      j   <- (r1.join, r2.join, r3.join, r4.join, r5.join, r6.join, r7.join, r8.join, r9.join, r10.join).parTupled
    } yield j

  final val case4: F[(Out[List[String]], Out[Unit], Out[Unit], Out[Unit], Out[List[String]], Out[Unit], Out[List[String]], Out[Unit], Out[Unit], Out[Unit])] =
    for {
      r1  <- shortChain1.start
      r2  <- shortChain2.start
      r3  <- shortChain3.start
      r4  <- shortChain4.start
      r5  <- shortChain5.start
      r6  <- shortChain6.start
      r7  <- shortChain7.start
      r8  <- shortChain8.start
      r9  <- shortChain9.start
      r10 <- shortChain10.start
      j   <- (r1.join, r2.join, r3.join, r4.join, r5.join, r6.join, r7.join, r8.join, r9.join, r10.join).parTupled
    } yield j

  final val case5: F[(Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit])] =
    for {
      r1 <- longDoubleChain1.start
      r2 <- longDoubleChain2.start
      r3 <- longDoubleChain3.start
      r4 <- longDoubleChain4.start
      r5 <- longDoubleChain5.start
      j  <- (r1.join, r2.join, r3.join, r4.join, r5.join).parTupled
    } yield j

  final val case6: F[(Out[Unit], Out[Unit], Out[Unit], Out[Unit], Out[Unit])] =
    for {
      r1 <- shortDoubleChain1.start
      r2 <- shortDoubleChain2.start
      r3 <- shortDoubleChain3.start
      r4 <- shortDoubleChain4.start
      r5 <- shortDoubleChain5.start
      j  <- (r1.join, r2.join, r3.join, r4.join, r5.join).parTupled
    } yield j
}

private[fs2] sealed abstract class TestCommandsRedisForCats[F[_]: FlatMap](cl: RedisCommands[F, String, String]) extends TestValues {

  protected final val longSend1: F[Long]          = cl.rPush("key11", longList1*)
  protected final val longSend2: F[Unit]          = cl.set("key12", longString1)
  protected final val longSend3: F[Unit]          = cl.set("key13", "17")
  protected final val longSend4: F[Unit]          = cl.mSet(longMulti1MapStr)
  protected final val longSend5: F[List[String]]  = cl.lRange("key11", 0L, 100L)
  protected final val longSend6: F[Unit]          = cl.set("key21", longString2)
  protected final val longSend7: F[Long]          = cl.rPush("key22", longList2*)
  protected final val longSend8: F[List[String]]  = cl.lRange("key22", 0L, 100L)
  protected final val longSend9: F[Unit]          = cl.set("key23", "17")
  protected final val longSend10: F[Unit]         = cl.mSet(longMulti2MapStr)
  protected final val longSend11: F[Long]         = cl.rPush("key31", longList3*)
  protected final val longSend12: F[Unit]         = cl.set("key32", longString3)
  protected final val longSend13: F[Unit]         = cl.mSet(longMulti3MapStr)
  protected final val longSend14: F[List[String]] = cl.lRange("key31", 0L, 100L)
  protected final val longSend15: F[Unit]         = cl.set("key33", "17")
  protected final val longSend16: F[Unit]         = cl.set("key41", longString4)
  protected final val longSend17: F[Unit]         = cl.set("key42", "17")
  protected final val longSend18: F[Long]         = cl.rPush("key43", longList4*)
  protected final val longSend19: F[List[String]] = cl.lRange("key43", 0L, 100L)
  protected final val longSend20: F[Unit]         = cl.mSet(longMulti4MapStr)
  protected final val longSend21: F[Long]         = cl.rPush("key51", longList5*)
  protected final val longSend22: F[Unit]         = cl.set("key52", longString5)
  protected final val longSend23: F[Unit]         = cl.set("key53", "17")
  protected final val longSend24: F[Unit]         = cl.mSet(longMulti5MapStr)
  protected final val longSend25: F[List[String]] = cl.lRange("key51", 0L, 100L)
  protected final val longSend26: F[Unit]         = cl.set("key61", longString6)
  protected final val longSend27: F[Long]         = cl.rPush("key62", longList6*)
  protected final val longSend28: F[List[String]] = cl.lRange("key62", 0L, 100L)
  protected final val longSend29: F[Unit]         = cl.set("key63", "17")
  protected final val longSend30: F[Unit]         = cl.mSet(longMulti6MapStr)
  protected final val longSend31: F[Unit]         = cl.set("key71", longString7)
  protected final val longSend32: F[Unit]         = cl.set("key72", "17")
  protected final val longSend33: F[Long]         = cl.rPush("key73", longList7*)
  protected final val longSend34: F[Unit]         = cl.mSet(longMulti7MapStr)
  protected final val longSend35: F[List[String]] = cl.lRange("key73", 0L, 100L)
  protected final val longSend36: F[Unit]         = cl.set("key81", longString8)
  protected final val longSend37: F[Unit]         = cl.set("key82", "17")
  protected final val longSend38: F[Long]         = cl.rPush("key83", longList8*)
  protected final val longSend39: F[List[String]] = cl.lRange("key83", 0L, 100L)
  protected final val longSend40: F[Unit]         = cl.mSet(longMulti8MapStr)
  protected final val longSend41: F[Unit]         = cl.lTrim("key11", 0L, 200L)
  protected final val longSend42: F[Unit]         = cl.lTrim("key22", 0L, 200L)
  protected final val longSend43: F[Unit]         = cl.lTrim("key31", 0L, 200L)
  protected final val longSend44: F[Unit]         = cl.lTrim("key43", 0L, 200L)
  protected final val longSend45: F[Unit]         = cl.lTrim("key51", 0L, 200L)
  protected final val longSend46: F[Unit]         = cl.lTrim("key62", 0L, 200L)
  protected final val longSend47: F[Unit]         = cl.lTrim("key73", 0L, 200L)
  protected final val longSend48: F[Unit]         = cl.lTrim("key83", 0L, 200L)

  protected final val shortSend1: F[Long]          = cl.rPush("key11", shortList1*)
  protected final val shortSend2: F[Unit]          = cl.set("key12", shortString1)
  protected final val shortSend3: F[Unit]          = cl.set("key13", "17")
  protected final val shortSend4: F[Unit]          = cl.mSet(shortMulti1MapStr)
  protected final val shortSend5: F[List[String]]  = cl.lRange("key11", 0L, 4L)
  protected final val shortSend6: F[Unit]          = cl.set("key21", shortString2)
  protected final val shortSend7: F[Long]          = cl.rPush("key22", shortList2*)
  protected final val shortSend8: F[List[String]]  = cl.lRange("key22", 0L, 4L)
  protected final val shortSend9: F[Unit]          = cl.set("key23", "17")
  protected final val shortSend10: F[Unit]         = cl.mSet(shortMulti2MapStr)
  protected final val shortSend11: F[Long]         = cl.rPush("key31", shortList3*)
  protected final val shortSend12: F[Unit]         = cl.set("key32", shortString3)
  protected final val shortSend13: F[Unit]         = cl.mSet(shortMulti3MapStr)
  protected final val shortSend14: F[List[String]] = cl.lRange("key31", 0L, 4L)
  protected final val shortSend15: F[Unit]         = cl.set("key33", "17")
  protected final val shortSend16: F[Unit]         = cl.set("key41", shortString4)
  protected final val shortSend17: F[Unit]         = cl.set("key42", "17")
  protected final val shortSend18: F[Long]         = cl.rPush("key43", shortList4*)
  protected final val shortSend19: F[List[String]] = cl.lRange("key43", 0L, 4L)
  protected final val shortSend20: F[Unit]         = cl.mSet(shortMulti4MapStr)
  protected final val shortSend21: F[Long]         = cl.rPush("key51", shortList5*)
  protected final val shortSend22: F[Unit]         = cl.set("key52", shortString5)
  protected final val shortSend23: F[Unit]         = cl.set("key53", "17")
  protected final val shortSend24: F[Unit]         = cl.mSet(shortMulti5MapStr)
  protected final val shortSend25: F[List[String]] = cl.lRange("key51", 0L, 4L)
  protected final val shortSend26: F[Unit]         = cl.set("key61", shortString6)
  protected final val shortSend27: F[Long]         = cl.rPush("key62", shortList6*)
  protected final val shortSend28: F[List[String]] = cl.lRange("key62", 0L, 4L)
  protected final val shortSend29: F[Unit]         = cl.set("key63", "17")
  protected final val shortSend30: F[Unit]         = cl.mSet(shortMulti6MapStr)
  protected final val shortSend31: F[Unit]         = cl.set("key71", shortString7)
  protected final val shortSend32: F[Unit]         = cl.set("key72", "17")
  protected final val shortSend33: F[Long]         = cl.rPush("key73", shortList7*)
  protected final val shortSend34: F[Unit]         = cl.mSet(shortMulti7MapStr)
  protected final val shortSend35: F[List[String]] = cl.lRange("key73", 0L, 4L)
  protected final val shortSend36: F[Unit]         = cl.set("key81", shortString8)
  protected final val shortSend37: F[Unit]         = cl.set("key82", "17")
  protected final val shortSend38: F[Long]         = cl.rPush("key83", shortList8*)
  protected final val shortSend39: F[List[String]] = cl.lRange("key83", 0L, 4L)
  protected final val shortSend40: F[Unit]         = cl.mSet(shortMulti8MapStr)
  protected final val shortSend41: F[Unit]         = cl.lTrim("key11", 0L, 5L)
  protected final val shortSend42: F[Unit]         = cl.lTrim("key22", 0L, 5L)
  protected final val shortSend43: F[Unit]         = cl.lTrim("key31", 0L, 5L)
  protected final val shortSend44: F[Unit]         = cl.lTrim("key43", 0L, 5L)
  protected final val shortSend45: F[Unit]         = cl.lTrim("key51", 0L, 5L)
  protected final val shortSend46: F[Unit]         = cl.lTrim("key62", 0L, 5L)
  protected final val shortSend47: F[Unit]         = cl.lTrim("key73", 0L, 5L)
  protected final val shortSend48: F[Unit]         = cl.lTrim("key83", 0L, 5L)

  protected final val longChain1: F[List[String]] = longSend1 >> longSend2 >> longSend3 >> longSend4 >> longSend5
  protected final val longChain2: F[Unit]         = longSend6 >> longSend7 >> longSend8 >> longSend9 >> longSend10
  protected final val longChain3: F[Unit]         = longSend11 >> longSend12 >> longSend13 >> longSend14 >> longSend15
  protected final val longChain4: F[Unit]         = longSend16 >> longSend17 >> longSend18 >> longSend19 >> longSend20
  protected final val longChain5: F[List[String]] = longSend21 >> longSend22 >> longSend23 >> longSend24 >> longSend25
  protected final val longChain6: F[Unit]         = longSend26 >> longSend27 >> longSend28 >> longSend29 >> longSend30
  protected final val longChain7: F[List[String]] = longSend31 >> longSend32 >> longSend33 >> longSend34 >> longSend25
  protected final val longChain8: F[Unit]         = longSend36 >> longSend37 >> longSend38 >> longSend39 >> longSend40
  protected final val longChain9: F[Unit]         = longSend40 >> longSend41 >> longSend42 >> longSend43 >> longSend44
  protected final val longChain10: F[Unit]        = longSend45 >> longSend46 >> longSend47 >> longSend48

  protected final val shortChain1: F[List[String]] = shortSend1 >> shortSend2 >> shortSend3 >> shortSend4 >> shortSend5
  protected final val shortChain2: F[Unit]         = shortSend6 >> shortSend7 >> shortSend8 >> shortSend9 >> shortSend10
  protected final val shortChain3: F[Unit]         = shortSend11 >> shortSend12 >> shortSend13 >> shortSend14 >> shortSend15
  protected final val shortChain4: F[Unit]         = shortSend16 >> shortSend17 >> shortSend18 >> shortSend19 >> shortSend20
  protected final val shortChain5: F[List[String]] = shortSend21 >> shortSend22 >> shortSend23 >> shortSend24 >> shortSend25
  protected final val shortChain6: F[Unit]         = shortSend26 >> shortSend27 >> shortSend28 >> shortSend29 >> shortSend30
  protected final val shortChain7: F[List[String]] = shortSend31 >> shortSend32 >> shortSend33 >> shortSend34 >> shortSend25
  protected final val shortChain8: F[Unit]         = shortSend36 >> shortSend37 >> shortSend38 >> shortSend39 >> shortSend40
  protected final val shortChain9: F[Unit]         = shortSend40 >> shortSend41 >> shortSend42 >> shortSend43 >> shortSend44
  protected final val shortChain10: F[Unit]        = shortSend45 >> shortSend46 >> shortSend47 >> shortSend48

  protected final val longDoubleChain1: F[Unit] = longChain1 >> longChain2
  protected final val longDoubleChain2: F[Unit] = longChain3 >> longChain4
  protected final val longDoubleChain3: F[Unit] = longChain5 >> longChain6
  protected final val longDoubleChain4: F[Unit] = longChain7 >> longChain8
  protected final val longDoubleChain5: F[Unit] = longChain9 >> longChain10

  protected final val shortDoubleChain1: F[Unit] = shortChain1 >> shortChain2
  protected final val shortDoubleChain2: F[Unit] = shortChain3 >> shortChain4
  protected final val shortDoubleChain3: F[Unit] = shortChain5 >> shortChain6
  protected final val shortDoubleChain4: F[Unit] = shortChain7 >> shortChain8
  protected final val shortDoubleChain5: F[Unit] = shortChain9 >> shortChain10
}
