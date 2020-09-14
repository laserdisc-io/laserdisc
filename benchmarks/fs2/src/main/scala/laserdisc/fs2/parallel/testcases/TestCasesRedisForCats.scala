package laserdisc
package fs2
package parallel
package testcases

import cats.effect.Concurrent
import cats.effect.syntax.concurrent._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.parallel._
import cats.{FlatMap, Parallel}
import dev.profunktor.redis4cats.RedisCommands

private[fs2] object RedisForCatsTestCases {
  final def apply[F[_]: Concurrent: Parallel](cl: RedisCommands[F, String, String]): RedisForCatsTestCases[F] =
    new RedisForCatsTestCases[F](cl) {}
}

private[fs2] sealed abstract class RedisForCatsTestCases[F[_]: Concurrent: Parallel](cl: RedisCommands[F, String, String])
    extends TestCommandsRedisForCats(cl) {

  final def case1 =
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

  final def case2 =
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

  final def case3 =
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

  final def case4 =
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

  final def case5 =
    for {
      r1 <- longDoubleChain1.start
      r2 <- longDoubleChain2.start
      r3 <- longDoubleChain3.start
      r4 <- longDoubleChain4.start
      r5 <- longDoubleChain5.start
      j  <- (r1.join, r2.join, r3.join, r4.join, r5.join).parTupled
    } yield j

  final def case6 =
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

  protected final def longSend1  = cl.rPush("key11", longList1: _*)
  protected final def longSend2  = cl.set("key12", longString1)
  protected final def longSend3  = cl.set("key13", "17")
  protected final def longSend4  = cl.mSet(longMulti1MapStr)
  protected final def longSend5  = cl.lRange("key11", 0L, 100L)
  protected final def longSend6  = cl.set("key21", longString2)
  protected final def longSend7  = cl.rPush("key22", longList2: _*)
  protected final def longSend8  = cl.lRange("key22", 0L, 100L)
  protected final def longSend9  = cl.set("key23", "17")
  protected final def longSend10 = cl.mSet(longMulti2MapStr)
  protected final def longSend11 = cl.rPush("key31", longList3: _*)
  protected final def longSend12 = cl.set("key32", longString3)
  protected final def longSend13 = cl.mSet(longMulti3MapStr)
  protected final def longSend14 = cl.lRange("key31", 0L, 100L)
  protected final def longSend15 = cl.set("key33", "17")
  protected final def longSend16 = cl.set("key41", longString4)
  protected final def longSend17 = cl.set("key42", "17")
  protected final def longSend18 = cl.rPush("key43", longList4: _*)
  protected final def longSend19 = cl.lRange("key43", 0L, 100L)
  protected final def longSend20 = cl.mSet(longMulti4MapStr)
  protected final def longSend21 = cl.rPush("key51", longList5: _*)
  protected final def longSend22 = cl.set("key52", longString5)
  protected final def longSend23 = cl.set("key53", "17")
  protected final def longSend24 = cl.mSet(longMulti5MapStr)
  protected final def longSend25 = cl.lRange("key51", 0L, 100L)
  protected final def longSend26 = cl.set("key61", longString6)
  protected final def longSend27 = cl.rPush("key62", longList6: _*)
  protected final def longSend28 = cl.lRange("key62", 0L, 100L)
  protected final def longSend29 = cl.set("key63", "17")
  protected final def longSend30 = cl.mSet(longMulti6MapStr)
  protected final def longSend31 = cl.set("key71", longString7)
  protected final def longSend32 = cl.set("key72", "17")
  protected final def longSend33 = cl.rPush("key73", longList7: _*)
  protected final def longSend34 = cl.mSet(longMulti7MapStr)
  protected final def longSend35 = cl.lRange("key73", 0L, 100L)
  protected final def longSend36 = cl.set("key81", longString8)
  protected final def longSend37 = cl.set("key82", "17")
  protected final def longSend38 = cl.rPush("key83", longList8: _*)
  protected final def longSend39 = cl.lRange("key83", 0L, 100L)
  protected final def longSend40 = cl.mSet(longMulti8MapStr)
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
  protected final def shortSend3  = cl.set("key13", "17")
  protected final def shortSend4  = cl.mSet(shortMulti1MapStr)
  protected final def shortSend5  = cl.lRange("key11", 0L, 4L)
  protected final def shortSend6  = cl.set("key21", shortString2)
  protected final def shortSend7  = cl.rPush("key22", shortList2: _*)
  protected final def shortSend8  = cl.lRange("key22", 0L, 4L)
  protected final def shortSend9  = cl.set("key23", "17")
  protected final def shortSend10 = cl.mSet(shortMulti2MapStr)
  protected final def shortSend11 = cl.rPush("key31", shortList3: _*)
  protected final def shortSend12 = cl.set("key32", shortString3)
  protected final def shortSend13 = cl.mSet(shortMulti3MapStr)
  protected final def shortSend14 = cl.lRange("key31", 0L, 4L)
  protected final def shortSend15 = cl.set("key33", "17")
  protected final def shortSend16 = cl.set("key41", shortString4)
  protected final def shortSend17 = cl.set("key42", "17")
  protected final def shortSend18 = cl.rPush("key43", shortList4: _*)
  protected final def shortSend19 = cl.lRange("key43", 0L, 4L)
  protected final def shortSend20 = cl.mSet(shortMulti4MapStr)
  protected final def shortSend21 = cl.rPush("key51", shortList5: _*)
  protected final def shortSend22 = cl.set("key52", shortString5)
  protected final def shortSend23 = cl.set("key53", "17")
  protected final def shortSend24 = cl.mSet(shortMulti5MapStr)
  protected final def shortSend25 = cl.lRange("key51", 0L, 4L)
  protected final def shortSend26 = cl.set("key61", shortString6)
  protected final def shortSend27 = cl.rPush("key62", shortList6: _*)
  protected final def shortSend28 = cl.lRange("key62", 0L, 4L)
  protected final def shortSend29 = cl.set("key63", "17")
  protected final def shortSend30 = cl.mSet(shortMulti6MapStr)
  protected final def shortSend31 = cl.set("key71", shortString7)
  protected final def shortSend32 = cl.set("key72", "17")
  protected final def shortSend33 = cl.rPush("key73", shortList7: _*)
  protected final def shortSend34 = cl.mSet(shortMulti7MapStr)
  protected final def shortSend35 = cl.lRange("key73", 0L, 4L)
  protected final def shortSend36 = cl.set("key81", shortString8)
  protected final def shortSend37 = cl.set("key82", "17")
  protected final def shortSend38 = cl.rPush("key83", shortList8: _*)
  protected final def shortSend39 = cl.lRange("key83", 0L, 4L)
  protected final def shortSend40 = cl.mSet(shortMulti8MapStr)
  protected final def shortSend41 = cl.lTrim("key11", 0L, 5L)
  protected final def shortSend42 = cl.lTrim("key22", 0L, 5L)
  protected final def shortSend43 = cl.lTrim("key31", 0L, 5L)
  protected final def shortSend44 = cl.lTrim("key43", 0L, 5L)
  protected final def shortSend45 = cl.lTrim("key51", 0L, 5L)
  protected final def shortSend46 = cl.lTrim("key62", 0L, 5L)
  protected final def shortSend47 = cl.lTrim("key73", 0L, 5L)
  protected final def shortSend48 = cl.lTrim("key83", 0L, 5L)

  protected final def longChain1  = longSend1 >> longSend2 >> longSend3 >> longSend4 >> longSend5
  protected final def longChain2  = longSend6 >> longSend7 >> longSend8 >> longSend9 >> longSend10
  protected final def longChain3  = longSend11 >> longSend12 >> longSend13 >> longSend14 >> longSend15
  protected final def longChain4  = longSend16 >> longSend17 >> longSend18 >> longSend19 >> longSend20
  protected final def longChain5  = longSend21 >> longSend22 >> longSend23 >> longSend24 >> longSend25
  protected final def longChain6  = longSend26 >> longSend27 >> longSend28 >> longSend29 >> longSend30
  protected final def longChain7  = longSend31 >> longSend32 >> longSend33 >> longSend34 >> longSend25
  protected final def longChain8  = longSend36 >> longSend37 >> longSend38 >> longSend39 >> longSend40
  protected final def longChain9  = longSend40 >> longSend41 >> longSend42 >> longSend43 >> longSend44
  protected final def longChain10 = longSend45 >> longSend46 >> longSend47 >> longSend48

  protected final def shortChain1  = shortSend1 >> shortSend2 >> shortSend3 >> shortSend4 >> shortSend5
  protected final def shortChain2  = shortSend6 >> shortSend7 >> shortSend8 >> shortSend9 >> shortSend10
  protected final def shortChain3  = shortSend11 >> shortSend12 >> shortSend13 >> shortSend14 >> shortSend15
  protected final def shortChain4  = shortSend16 >> shortSend17 >> shortSend18 >> shortSend19 >> shortSend20
  protected final def shortChain5  = shortSend21 >> shortSend22 >> shortSend23 >> shortSend24 >> shortSend25
  protected final def shortChain6  = shortSend26 >> shortSend27 >> shortSend28 >> shortSend29 >> shortSend30
  protected final def shortChain7  = shortSend31 >> shortSend32 >> shortSend33 >> shortSend34 >> shortSend25
  protected final def shortChain8  = shortSend36 >> shortSend37 >> shortSend38 >> shortSend39 >> shortSend40
  protected final def shortChain9  = shortSend40 >> shortSend41 >> shortSend42 >> shortSend43 >> shortSend44
  protected final def shortChain10 = shortSend45 >> shortSend46 >> shortSend47 >> shortSend48

  protected final def longDoubleChain1 = longChain1 >> longChain2
  protected final def longDoubleChain2 = longChain3 >> longChain4
  protected final def longDoubleChain3 = longChain5 >> longChain6
  protected final def longDoubleChain4 = longChain7 >> longChain8
  protected final def longDoubleChain5 = longChain9 >> longChain10

  protected final def shortDoubleChain1 = shortChain1 >> shortChain2
  protected final def shortDoubleChain2 = shortChain3 >> shortChain4
  protected final def shortDoubleChain3 = shortChain5 >> shortChain6
  protected final def shortDoubleChain4 = shortChain7 >> shortChain8
  protected final def shortDoubleChain5 = shortChain9 >> shortChain10
}
