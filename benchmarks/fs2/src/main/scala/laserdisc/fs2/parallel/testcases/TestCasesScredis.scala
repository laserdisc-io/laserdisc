package laserdisc
package fs2
package parallel
package testcases

import scredis.Redis

import scala.concurrent.ExecutionContext

private[fs2] object TestCasesScredis {
  final def apply(cl: Redis)(implicit ec: ExecutionContext): TestCasesScredis =
    new TestCasesScredis(cl) {}
}

private[fs2] sealed abstract class TestCasesScredis(cl: Redis)(implicit ec: ExecutionContext) extends ScredisCommands(cl) {

  final def case1 =
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

  final def case2 =
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

  final def case3 =
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

  final def case4 =
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

  final def case5 =
    for {
      r1 <- longDoubleChain1
      r2 <- longDoubleChain2
      r3 <- longDoubleChain3
      r4 <- longDoubleChain4
      r5 <- longDoubleChain5
    } yield (r1, r2, r3, r4, r5)

  final def case6 =
    for {
      r1 <- shortDoubleChain1
      r2 <- shortDoubleChain2
      r3 <- shortDoubleChain3
      r4 <- shortDoubleChain4
      r5 <- shortDoubleChain5
    } yield (r1, r2, r3, r4, r5)
}

private[fs2] sealed abstract class ScredisCommands(cl: Redis)(implicit ec: ExecutionContext) extends TestValues {

  protected final def longSend1  = cl.rPush("key11", longList1: _*)
  protected final def longSend2  = cl.set("key12", longString1)
  protected final def longSend3  = cl.set("key13", 17)
  protected final def longSend4  = cl.mSet(longMulti1Map)
  protected final def longSend5  = cl.lRange("key11", 0L, 100L)
  protected final def longSend6  = cl.set("key21", longString2)
  protected final def longSend7  = cl.rPush("key22", longList2: _*)
  protected final def longSend8  = cl.lRange("key22", 0L, 100L)
  protected final def longSend9  = cl.set("key23", 17)
  protected final def longSend10 = cl.mSet(longMulti2Map)
  protected final def longSend11 = cl.rPush("key31", longList3: _*)
  protected final def longSend12 = cl.set("key32", longString3)
  protected final def longSend13 = cl.mSet(longMulti3Map)
  protected final def longSend14 = cl.lRange("key31", 0L, 100L)
  protected final def longSend15 = cl.set("key33", 17)
  protected final def longSend16 = cl.set("key41", longString4)
  protected final def longSend17 = cl.set("key42", 17)
  protected final def longSend18 = cl.rPush("key43", longList4: _*)
  protected final def longSend19 = cl.lRange("key43", 0L, 100L)
  protected final def longSend20 = cl.mSet(longMulti4Map)
  protected final def longSend21 = cl.rPush("key51", longList5: _*)
  protected final def longSend22 = cl.set("key52", longString5)
  protected final def longSend23 = cl.set("key53", 17)
  protected final def longSend24 = cl.mSet(longMulti5Map)
  protected final def longSend25 = cl.lRange("key51", 0L, 100L)
  protected final def longSend26 = cl.set("key61", longString6)
  protected final def longSend27 = cl.rPush("key62", longList6: _*)
  protected final def longSend28 = cl.lRange("key62", 0L, 100L)
  protected final def longSend29 = cl.set("key63", 17)
  protected final def longSend30 = cl.mSet(longMulti6Map)
  protected final def longSend31 = cl.set("key71", longString7)
  protected final def longSend32 = cl.set("key72", 17)
  protected final def longSend33 = cl.rPush("key73", longList7: _*)
  protected final def longSend34 = cl.mSet(longMulti7Map)
  protected final def longSend35 = cl.lRange("key73", 0L, 100L)
  protected final def longSend36 = cl.set("key81", longString8)
  protected final def longSend37 = cl.set("key82", 17)
  protected final def longSend38 = cl.rPush("key83", longList8: _*)
  protected final def longSend39 = cl.lRange("key83", 0L, 100L)
  protected final def longSend40 = cl.mSet(longMulti8Map)
  protected final def longSend41 = cl.lTrim("key11", 0L, 200L)
  protected final def longSend42 = cl.lTrim("key22", 0L, 200L)
  protected final def longSend43 = cl.lTrim("key31", 0L, 200L)
  protected final def longSend44 = cl.lTrim("key43", 0L, 200L)
  protected final def longSend45 = cl.lTrim("key51", 0L, 200L)
  protected final def longSend46 = cl.lTrim("key62", 0L, 200L)
  protected final def longSend47 = cl.lTrim("key73", 0L, 200L)
  protected final def longSend48 = cl.lTrim("key83", 0L, 200L)

  protected final def shortSend1  = cl.rPush("key11", shortList1: _*)
  protected final def shortSend2  = cl.set("key12", shortString1)
  protected final def shortSend3  = cl.set("key13", 17)
  protected final def shortSend4  = cl.mSet(shortMulti1Map)
  protected final def shortSend5  = cl.lRange("key11", 0L, 4L)
  protected final def shortSend6  = cl.set("key21", shortString2)
  protected final def shortSend7  = cl.rPush("key22", shortList2: _*)
  protected final def shortSend8  = cl.lRange("key22", 0L, 4L)
  protected final def shortSend9  = cl.set("key23", 17)
  protected final def shortSend10 = cl.mSet(shortMulti2Map)
  protected final def shortSend11 = cl.rPush("key31", shortList3: _*)
  protected final def shortSend12 = cl.set("key32", shortString3)
  protected final def shortSend13 = cl.mSet(shortMulti3Map)
  protected final def shortSend14 = cl.lRange("key31", 0L, 4L)
  protected final def shortSend15 = cl.set("key33", 17)
  protected final def shortSend16 = cl.set("key41", shortString4)
  protected final def shortSend17 = cl.set("key42", 17)
  protected final def shortSend18 = cl.rPush("key43", shortList4: _*)
  protected final def shortSend19 = cl.lRange("key43", 0L, 4L)
  protected final def shortSend20 = cl.mSet(shortMulti4Map)
  protected final def shortSend21 = cl.rPush("key51", shortList5: _*)
  protected final def shortSend22 = cl.set("key52", shortString5)
  protected final def shortSend23 = cl.set("key53", 17)
  protected final def shortSend24 = cl.mSet(shortMulti5Map)
  protected final def shortSend25 = cl.lRange("key51", 0L, 4L)
  protected final def shortSend26 = cl.set("key61", shortString6)
  protected final def shortSend27 = cl.rPush("key62", shortList6: _*)
  protected final def shortSend28 = cl.lRange("key62", 0L, 4L)
  protected final def shortSend29 = cl.set("key63", 17)
  protected final def shortSend30 = cl.mSet(shortMulti6Map)
  protected final def shortSend31 = cl.set("key71", shortString7)
  protected final def shortSend32 = cl.set("key72", 17)
  protected final def shortSend33 = cl.rPush("key73", shortList7: _*)
  protected final def shortSend34 = cl.mSet(shortMulti7Map)
  protected final def shortSend35 = cl.lRange("key73", 0L, 4L)
  protected final def shortSend36 = cl.set("key81", shortString8)
  protected final def shortSend37 = cl.set("key82", 17)
  protected final def shortSend38 = cl.rPush("key83", shortList8: _*)
  protected final def shortSend39 = cl.lRange("key83", 0L, 4L)
  protected final def shortSend40 = cl.mSet(shortMulti8Map)
  protected final def shortSend41 = cl.lTrim("key11", 0L, 5L)
  protected final def shortSend42 = cl.lTrim("key22", 0L, 5L)
  protected final def shortSend43 = cl.lTrim("key31", 0L, 5L)
  protected final def shortSend44 = cl.lTrim("key43", 0L, 5L)
  protected final def shortSend45 = cl.lTrim("key51", 0L, 5L)
  protected final def shortSend46 = cl.lTrim("key62", 0L, 5L)
  protected final def shortSend47 = cl.lTrim("key73", 0L, 5L)
  protected final def shortSend48 = cl.lTrim("key83", 0L, 5L)

  protected final def longChain1 = longSend1.flatMap(_ => longSend2).flatMap(_ => longSend3).flatMap(_ => longSend4).flatMap(_ => longSend5)
  protected final def longChain2 =
    longSend6.flatMap(_ => longSend7).flatMap(_ => longSend8).flatMap(_ => longSend9).flatMap(_ => longSend10)
  protected final def longChain3 =
    longSend11.flatMap(_ => longSend12).flatMap(_ => longSend13).flatMap(_ => longSend14).flatMap(_ => longSend15)
  protected final def longChain4 =
    longSend16.flatMap(_ => longSend17).flatMap(_ => longSend18).flatMap(_ => longSend19).flatMap(_ => longSend20)
  protected final def longChain5 =
    longSend21.flatMap(_ => longSend22).flatMap(_ => longSend23).flatMap(_ => longSend24).flatMap(_ => longSend25)
  protected final def longChain6 =
    longSend26.flatMap(_ => longSend27).flatMap(_ => longSend28).flatMap(_ => longSend29).flatMap(_ => longSend30)
  protected final def longChain7 =
    longSend31.flatMap(_ => longSend32).flatMap(_ => longSend33).flatMap(_ => longSend34).flatMap(_ => longSend35)
  protected final def longChain8 =
    longSend36.flatMap(_ => longSend37).flatMap(_ => longSend38).flatMap(_ => longSend39).flatMap(_ => longSend40)
  protected final def longChain9 =
    longSend41.flatMap(_ => longSend42).flatMap(_ => longSend43).flatMap(_ => longSend44).flatMap(_ => longSend45)
  protected final def longChain10 =
    longSend46.flatMap(_ => longSend47).flatMap(_ => longSend48)

  protected final def shortChain1 =
    shortSend1.flatMap(_ => shortSend2).flatMap(_ => shortSend3).flatMap(_ => shortSend4).flatMap(_ => shortSend5)
  protected final def shortChain2 =
    shortSend6.flatMap(_ => shortSend7).flatMap(_ => shortSend8).flatMap(_ => shortSend9).flatMap(_ => shortSend10)
  protected final def shortChain3 =
    shortSend11.flatMap(_ => shortSend12).flatMap(_ => shortSend13).flatMap(_ => shortSend14).flatMap(_ => shortSend15)
  protected final def shortChain4 =
    shortSend16.flatMap(_ => shortSend17).flatMap(_ => shortSend18).flatMap(_ => shortSend19).flatMap(_ => shortSend20)
  protected final def shortChain5 =
    shortSend21.flatMap(_ => shortSend22).flatMap(_ => shortSend23).flatMap(_ => shortSend24).flatMap(_ => shortSend25)
  protected final def shortChain6 =
    shortSend26.flatMap(_ => shortSend27).flatMap(_ => shortSend28).flatMap(_ => shortSend29).flatMap(_ => shortSend30)
  protected final def shortChain7 =
    shortSend31.flatMap(_ => shortSend32).flatMap(_ => shortSend33).flatMap(_ => shortSend34).flatMap(_ => shortSend35)
  protected final def shortChain8 =
    shortSend36.flatMap(_ => shortSend37).flatMap(_ => shortSend38).flatMap(_ => shortSend39).flatMap(_ => shortSend40)
  protected final def shortChain9 =
    shortSend41.flatMap(_ => shortSend42).flatMap(_ => shortSend43).flatMap(_ => shortSend44).flatMap(_ => shortSend45)
  protected final def shortChain10 =
    shortSend46.flatMap(_ => shortSend47).flatMap(_ => shortSend48)

  protected final def longDoubleChain1 = longChain1.flatMap(_ => longChain2)
  protected final def longDoubleChain2 = longChain3.flatMap(_ => longChain4)
  protected final def longDoubleChain3 = longChain5.flatMap(_ => longChain6)
  protected final def longDoubleChain4 = longChain7.flatMap(_ => longChain8)
  protected final def longDoubleChain5 = longChain9.flatMap(_ => longChain10)

  protected final def shortDoubleChain1 = shortChain1.flatMap(_ => shortChain2)
  protected final def shortDoubleChain2 = shortChain3.flatMap(_ => shortChain4)
  protected final def shortDoubleChain3 = shortChain5.flatMap(_ => shortChain6)
  protected final def shortDoubleChain4 = shortChain7.flatMap(_ => shortChain8)
  protected final def shortDoubleChain5 = shortChain9.flatMap(_ => shortChain10)
}
