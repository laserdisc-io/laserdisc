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

import scredis.*

import scala.concurrent.{ExecutionContext, Future}

private[fs2] object TestCasesScredis {
  final def apply(cl: Redis)(implicit ec: ExecutionContext): TestCasesScredis =
    new TestCasesScredis(cl) {}
}

private[fs2] sealed abstract class TestCasesScredis(cl: Redis)(implicit ec: ExecutionContext) extends ScredisCommands(cl) {

  final def case1: Future[((Long, Boolean, Boolean, Unit, List[String], Boolean, Long, List[String], Boolean, Unit, Long, Boolean, Unit, List[String], Boolean, Boolean, Boolean, Long, List[String], Unit), (Long, Boolean, Boolean, Unit, List[String], Boolean, Long, List[String], Boolean, Unit, Boolean, Boolean, Long, Unit, List[String], Boolean, Boolean, Long, List[String], Unit), (Unit, Unit, Unit, Unit, Unit, Unit, Unit, Unit))] =
    for {
      r1  <- longSend1
      r2  <- longSend2
      r3  <- longSend3
      r4  <- longSend4
      r5  <- longSend5
      r6  <- longSend6
      r7  <- longSend7
      r8  <- longSend8
      r9  <- longSend9
      r10 <- longSend10
      r11 <- longSend11
      r12 <- longSend12
      r13 <- longSend13
      r14 <- longSend14
      r15 <- longSend15
      r16 <- longSend16
      r17 <- longSend17
      r18 <- longSend18
      r19 <- longSend19
      r20 <- longSend20
      r21 <- longSend21
      r22 <- longSend22
      r23 <- longSend23
      r24 <- longSend24
      r25 <- longSend25
      r26 <- longSend26
      r27 <- longSend27
      r28 <- longSend28
      r29 <- longSend29
      r30 <- longSend30
      r31 <- longSend31
      r32 <- longSend32
      r33 <- longSend33
      r34 <- longSend34
      r35 <- longSend35
      r36 <- longSend36
      r37 <- longSend37
      r38 <- longSend38
      r39 <- longSend39
      r40 <- longSend40
      r41 <- longSend41
      r42 <- longSend42
      r43 <- longSend43
      r44 <- longSend44
      r45 <- longSend45
      r46 <- longSend46
      r47 <- longSend47
      r48 <- longSend48
    } yield (
      (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20),
      (r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40),
      (r41, r42, r43, r44, r45, r46, r47, r48)
    )

  final def case2: Future[((Long, Boolean, Boolean, Unit, List[String], Boolean, Long, List[String], Boolean, Unit, Long, Boolean, Unit, List[String], Boolean, Boolean, Boolean, Long, List[String], Unit), (Long, Boolean, Boolean, Unit, List[String], Boolean, Long, List[String], Boolean, Unit, Boolean, Boolean, Long, Unit, List[String], Boolean, Boolean, Long, List[String], Unit), (Unit, Unit, Unit, Unit, Unit, Unit, Unit, Unit))] =
    for {
      r1  <- shortSend1
      r2  <- shortSend2
      r3  <- shortSend3
      r4  <- shortSend4
      r5  <- shortSend5
      r6  <- shortSend6
      r7  <- shortSend7
      r8  <- shortSend8
      r9  <- shortSend9
      r10 <- shortSend10
      r11 <- shortSend11
      r12 <- shortSend12
      r13 <- shortSend13
      r14 <- shortSend14
      r15 <- shortSend15
      r16 <- shortSend16
      r17 <- shortSend17
      r18 <- shortSend18
      r19 <- shortSend19
      r20 <- shortSend20
      r21 <- shortSend21
      r22 <- shortSend22
      r23 <- shortSend23
      r24 <- shortSend24
      r25 <- shortSend25
      r26 <- shortSend26
      r27 <- shortSend27
      r28 <- shortSend28
      r29 <- shortSend29
      r30 <- shortSend30
      r31 <- shortSend31
      r32 <- shortSend32
      r33 <- shortSend33
      r34 <- shortSend34
      r35 <- shortSend35
      r36 <- shortSend36
      r37 <- shortSend37
      r38 <- shortSend38
      r39 <- shortSend39
      r40 <- shortSend40
      r41 <- shortSend41
      r42 <- shortSend42
      r43 <- shortSend43
      r44 <- shortSend44
      r45 <- shortSend45
      r46 <- shortSend46
      r47 <- shortSend47
      r48 <- shortSend48
    } yield (
      (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20),
      (r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40),
      (r41, r42, r43, r44, r45, r46, r47, r48)
    )

  final def case3: Future[(List[String], Unit, Boolean, Unit, List[String], Unit, List[String], Unit, Unit, Unit)] =
    for {
      r1  <- longChain1
      r2  <- longChain2
      r3  <- longChain3
      r4  <- longChain4
      r5  <- longChain5
      r6  <- longChain6
      r7  <- longChain7
      r8  <- longChain8
      r9  <- longChain9
      r10 <- longChain10
    } yield (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)

  final def case4: Future[(List[String], Unit, Boolean, Unit, List[String], Unit, List[String], Unit, Unit, Unit)] =
    for {
      r1  <- shortChain1
      r2  <- shortChain2
      r3  <- shortChain3
      r4  <- shortChain4
      r5  <- shortChain5
      r6  <- shortChain6
      r7  <- shortChain7
      r8  <- shortChain8
      r9  <- shortChain9
      r10 <- shortChain10
    } yield (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)

  final def case5: Future[(Unit, Unit, Unit, Unit, Unit)] =
    for {
      r1 <- longDoubleChain1
      r2 <- longDoubleChain2
      r3 <- longDoubleChain3
      r4 <- longDoubleChain4
      r5 <- longDoubleChain5
    } yield (r1, r2, r3, r4, r5)

  final def case6: Future[(Unit, Unit, Unit, Unit, Unit)] =
    for {
      r1 <- shortDoubleChain1
      r2 <- shortDoubleChain2
      r3 <- shortDoubleChain3
      r4 <- shortDoubleChain4
      r5 <- shortDoubleChain5
    } yield (r1, r2, r3, r4, r5)
}

private[fs2] sealed abstract class ScredisCommands(cl: Redis)(implicit ec: ExecutionContext) extends TestValues {

  protected final def longSend1: Future[Long]          = cl.rPush("key11", longList1: _*)
  protected final def longSend2: Future[Boolean]       = cl.set("key12", longString1)
  protected final def longSend3: Future[Boolean]       = cl.set("key13", 17)
  protected final def longSend4: Future[Unit]          = cl.mSet(longMulti1Map)
  protected final def longSend5: Future[List[String]]  = cl.lRange("key11", 0L, 100L)
  protected final def longSend6: Future[Boolean]       = cl.set("key21", longString2)
  protected final def longSend7: Future[Long]          = cl.rPush("key22", longList2: _*)
  protected final def longSend8: Future[List[String]]  = cl.lRange("key22", 0L, 100L)
  protected final def longSend9: Future[Boolean]       = cl.set("key23", 17)
  protected final def longSend10: Future[Unit]         = cl.mSet(longMulti2Map)
  protected final def longSend11: Future[Long]         = cl.rPush("key31", longList3: _*)
  protected final def longSend12: Future[Boolean]      = cl.set("key32", longString3)
  protected final def longSend13: Future[Unit]         = cl.mSet(longMulti3Map)
  protected final def longSend14: Future[List[String]] = cl.lRange("key31", 0L, 100L)
  protected final def longSend15: Future[Boolean]      = cl.set("key33", 17)
  protected final def longSend16: Future[Boolean]      = cl.set("key41", longString4)
  protected final def longSend17: Future[Boolean]      = cl.set("key42", 17)
  protected final def longSend18: Future[Long]         = cl.rPush("key43", longList4: _*)
  protected final def longSend19: Future[List[String]] = cl.lRange("key43", 0L, 100L)
  protected final def longSend20: Future[Unit]         = cl.mSet(longMulti4Map)
  protected final def longSend21: Future[Long]         = cl.rPush("key51", longList5: _*)
  protected final def longSend22: Future[Boolean]      = cl.set("key52", longString5)
  protected final def longSend23: Future[Boolean]      = cl.set("key53", 17)
  protected final def longSend24: Future[Unit]         = cl.mSet(longMulti5Map)
  protected final def longSend25: Future[List[String]] = cl.lRange("key51", 0L, 100L)
  protected final def longSend26: Future[Boolean]      = cl.set("key61", longString6)
  protected final def longSend27: Future[Long]         = cl.rPush("key62", longList6: _*)
  protected final def longSend28: Future[List[String]] = cl.lRange("key62", 0L, 100L)
  protected final def longSend29: Future[Boolean]      = cl.set("key63", 17)
  protected final def longSend30: Future[Unit]         = cl.mSet(longMulti6Map)
  protected final def longSend31: Future[Boolean]      = cl.set("key71", longString7)
  protected final def longSend32: Future[Boolean]      = cl.set("key72", 17)
  protected final def longSend33: Future[Long]         = cl.rPush("key73", longList7: _*)
  protected final def longSend34: Future[Unit]         = cl.mSet(longMulti7Map)
  protected final def longSend35: Future[List[String]] = cl.lRange("key73", 0L, 100L)
  protected final def longSend36: Future[Boolean]      = cl.set("key81", longString8)
  protected final def longSend37: Future[Boolean]      = cl.set("key82", 17)
  protected final def longSend38: Future[Long]         = cl.rPush("key83", longList8: _*)
  protected final def longSend39: Future[List[String]] = cl.lRange("key83", 0L, 100L)
  protected final def longSend40: Future[Unit]         = cl.mSet(longMulti8Map)
  protected final def longSend41: Future[Unit]         = cl.lTrim("key11", 0L, 200L)
  protected final def longSend42: Future[Unit]         = cl.lTrim("key22", 0L, 200L)
  protected final def longSend43: Future[Unit]         = cl.lTrim("key31", 0L, 200L)
  protected final def longSend44: Future[Unit]         = cl.lTrim("key43", 0L, 200L)
  protected final def longSend45: Future[Unit]         = cl.lTrim("key51", 0L, 200L)
  protected final def longSend46: Future[Unit]         = cl.lTrim("key62", 0L, 200L)
  protected final def longSend47: Future[Unit]         = cl.lTrim("key73", 0L, 200L)
  protected final def longSend48: Future[Unit]         = cl.lTrim("key83", 0L, 200L)

  protected final def shortSend1: Future[Long]          = cl.rPush("key11", shortList1: _*)
  protected final def shortSend2: Future[Boolean]       = cl.set("key12", shortString1)
  protected final def shortSend3: Future[Boolean]       = cl.set("key13", 17)
  protected final def shortSend4: Future[Unit]          = cl.mSet(shortMulti1Map)
  protected final def shortSend5: Future[List[String]]  = cl.lRange("key11", 0L, 4L)
  protected final def shortSend6: Future[Boolean]       = cl.set("key21", shortString2)
  protected final def shortSend7: Future[Long]          = cl.rPush("key22", shortList2: _*)
  protected final def shortSend8: Future[List[String]]  = cl.lRange("key22", 0L, 4L)
  protected final def shortSend9: Future[Boolean]       = cl.set("key23", 17)
  protected final def shortSend10: Future[Unit]         = cl.mSet(shortMulti2Map)
  protected final def shortSend11: Future[Long]         = cl.rPush("key31", shortList3: _*)
  protected final def shortSend12: Future[Boolean]      = cl.set("key32", shortString3)
  protected final def shortSend13: Future[Unit]         = cl.mSet(shortMulti3Map)
  protected final def shortSend14: Future[List[String]] = cl.lRange("key31", 0L, 4L)
  protected final def shortSend15: Future[Boolean]      = cl.set("key33", 17)
  protected final def shortSend16: Future[Boolean]      = cl.set("key41", shortString4)
  protected final def shortSend17: Future[Boolean]      = cl.set("key42", 17)
  protected final def shortSend18: Future[Long]         = cl.rPush("key43", shortList4: _*)
  protected final def shortSend19: Future[List[String]] = cl.lRange("key43", 0L, 4L)
  protected final def shortSend20: Future[Unit]         = cl.mSet(shortMulti4Map)
  protected final def shortSend21: Future[Long]         = cl.rPush("key51", shortList5: _*)
  protected final def shortSend22: Future[Boolean]      = cl.set("key52", shortString5)
  protected final def shortSend23: Future[Boolean]      = cl.set("key53", 17)
  protected final def shortSend24: Future[Unit]         = cl.mSet(shortMulti5Map)
  protected final def shortSend25: Future[List[String]] = cl.lRange("key51", 0L, 4L)
  protected final def shortSend26: Future[Boolean]      = cl.set("key61", shortString6)
  protected final def shortSend27: Future[Long]         = cl.rPush("key62", shortList6: _*)
  protected final def shortSend28: Future[List[String]] = cl.lRange("key62", 0L, 4L)
  protected final def shortSend29: Future[Boolean]      = cl.set("key63", 17)
  protected final def shortSend30: Future[Unit]         = cl.mSet(shortMulti6Map)
  protected final def shortSend31: Future[Boolean]      = cl.set("key71", shortString7)
  protected final def shortSend32: Future[Boolean]      = cl.set("key72", 17)
  protected final def shortSend33: Future[Long]         = cl.rPush("key73", shortList7: _*)
  protected final def shortSend34: Future[Unit]         = cl.mSet(shortMulti7Map)
  protected final def shortSend35: Future[List[String]] = cl.lRange("key73", 0L, 4L)
  protected final def shortSend36: Future[Boolean]      = cl.set("key81", shortString8)
  protected final def shortSend37: Future[Boolean]      = cl.set("key82", 17)
  protected final def shortSend38: Future[Long]         = cl.rPush("key83", shortList8: _*)
  protected final def shortSend39: Future[List[String]] = cl.lRange("key83", 0L, 4L)
  protected final def shortSend40: Future[Unit]         = cl.mSet(shortMulti8Map)
  protected final def shortSend41: Future[Unit]         = cl.lTrim("key11", 0L, 5L)
  protected final def shortSend42: Future[Unit]         = cl.lTrim("key22", 0L, 5L)
  protected final def shortSend43: Future[Unit]         = cl.lTrim("key31", 0L, 5L)
  protected final def shortSend44: Future[Unit]         = cl.lTrim("key43", 0L, 5L)
  protected final def shortSend45: Future[Unit]         = cl.lTrim("key51", 0L, 5L)
  protected final def shortSend46: Future[Unit]         = cl.lTrim("key62", 0L, 5L)
  protected final def shortSend47: Future[Unit]         = cl.lTrim("key73", 0L, 5L)
  protected final def shortSend48: Future[Unit]         = cl.lTrim("key83", 0L, 5L)

  protected final def longChain1: Future[List[String]] =
    longSend1.flatMap(_ => longSend2).flatMap(_ => longSend3).flatMap(_ => longSend4).flatMap(_ => longSend5)
  protected final def longChain2: Future[Unit] =
    longSend6.flatMap(_ => longSend7).flatMap(_ => longSend8).flatMap(_ => longSend9).flatMap(_ => longSend10)
  protected final def longChain3: Future[Boolean] =
    longSend11.flatMap(_ => longSend12).flatMap(_ => longSend13).flatMap(_ => longSend14).flatMap(_ => longSend15)
  protected final def longChain4: Future[Unit] =
    longSend16.flatMap(_ => longSend17).flatMap(_ => longSend18).flatMap(_ => longSend19).flatMap(_ => longSend20)
  protected final def longChain5: Future[List[String]] =
    longSend21.flatMap(_ => longSend22).flatMap(_ => longSend23).flatMap(_ => longSend24).flatMap(_ => longSend25)
  protected final def longChain6: Future[Unit] =
    longSend26.flatMap(_ => longSend27).flatMap(_ => longSend28).flatMap(_ => longSend29).flatMap(_ => longSend30)
  protected final def longChain7: Future[List[String]] =
    longSend31.flatMap(_ => longSend32).flatMap(_ => longSend33).flatMap(_ => longSend34).flatMap(_ => longSend35)
  protected final def longChain8: Future[Unit] =
    longSend36.flatMap(_ => longSend37).flatMap(_ => longSend38).flatMap(_ => longSend39).flatMap(_ => longSend40)
  protected final def longChain9: Future[Unit] =
    longSend41.flatMap(_ => longSend42).flatMap(_ => longSend43).flatMap(_ => longSend44).flatMap(_ => longSend45)
  protected final def longChain10: Future[Unit] =
    longSend46.flatMap(_ => longSend47).flatMap(_ => longSend48)

  protected final def shortChain1: Future[List[String]] =
    shortSend1.flatMap(_ => shortSend2).flatMap(_ => shortSend3).flatMap(_ => shortSend4).flatMap(_ => shortSend5)
  protected final def shortChain2: Future[Unit] =
    shortSend6.flatMap(_ => shortSend7).flatMap(_ => shortSend8).flatMap(_ => shortSend9).flatMap(_ => shortSend10)
  protected final def shortChain3: Future[Boolean] =
    shortSend11.flatMap(_ => shortSend12).flatMap(_ => shortSend13).flatMap(_ => shortSend14).flatMap(_ => shortSend15)
  protected final def shortChain4: Future[Unit] =
    shortSend16.flatMap(_ => shortSend17).flatMap(_ => shortSend18).flatMap(_ => shortSend19).flatMap(_ => shortSend20)
  protected final def shortChain5: Future[List[String]] =
    shortSend21.flatMap(_ => shortSend22).flatMap(_ => shortSend23).flatMap(_ => shortSend24).flatMap(_ => shortSend25)
  protected final def shortChain6: Future[Unit] =
    shortSend26.flatMap(_ => shortSend27).flatMap(_ => shortSend28).flatMap(_ => shortSend29).flatMap(_ => shortSend30)
  protected final def shortChain7: Future[List[String]] =
    shortSend31.flatMap(_ => shortSend32).flatMap(_ => shortSend33).flatMap(_ => shortSend34).flatMap(_ => shortSend35)
  protected final def shortChain8: Future[Unit] =
    shortSend36.flatMap(_ => shortSend37).flatMap(_ => shortSend38).flatMap(_ => shortSend39).flatMap(_ => shortSend40)
  protected final def shortChain9: Future[Unit] =
    shortSend41.flatMap(_ => shortSend42).flatMap(_ => shortSend43).flatMap(_ => shortSend44).flatMap(_ => shortSend45)
  protected final def shortChain10: Future[Unit] =
    shortSend46.flatMap(_ => shortSend47).flatMap(_ => shortSend48)

  protected final def longDoubleChain1: Future[Unit] = longChain1.flatMap(_ => longChain2)
  protected final def longDoubleChain2: Future[Unit] = longChain3.flatMap(_ => longChain4)
  protected final def longDoubleChain3: Future[Unit] = longChain5.flatMap(_ => longChain6)
  protected final def longDoubleChain4: Future[Unit] = longChain7.flatMap(_ => longChain8)
  protected final def longDoubleChain5: Future[Unit] = longChain9.flatMap(_ => longChain10)

  protected final def shortDoubleChain1: Future[Unit] = shortChain1.flatMap(_ => shortChain2)
  protected final def shortDoubleChain2: Future[Unit] = shortChain3.flatMap(_ => shortChain4)
  protected final def shortDoubleChain3: Future[Unit] = shortChain5.flatMap(_ => shortChain6)
  protected final def shortDoubleChain4: Future[Unit] = shortChain7.flatMap(_ => shortChain8)
  protected final def shortDoubleChain5: Future[Unit] = shortChain9.flatMap(_ => shortChain10)
}
