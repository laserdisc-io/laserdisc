package laserdisc
package fs2
package parallel
package testcases

import cats.Parallel
import cats.effect.syntax.concurrent._
import cats.effect.{Concurrent, ContextShift, Timer}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.parallel._

private[fs2] object TestCasesLaserdisc {
  final def apply[F[_]: Concurrent: ContextShift: Timer: Parallel](cl: RedisClient[F]): TestCasesLaserdisc[F] =
    new TestCasesLaserdisc[F](cl) {}
}
private[fs2] sealed abstract class TestCasesLaserdisc[F[_]: Concurrent: ContextShift: Timer: Parallel](cl: RedisClient[F])
    extends TestSendLaserdisc(cl) {

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

private[fs2] sealed abstract class TestSendLaserdisc[F[_]: Concurrent: ContextShift: Timer: Parallel](cl: RedisClient[F])
    extends TestCommandsProtocol {

  protected final def longSend1  = cl.send(longCmd1)
  protected final def longSend2  = cl.send(longCmd2)
  protected final def longSend3  = cl.send(longCmd3)
  protected final def longSend4  = cl.send(longCmd4)
  protected final def longSend5  = cl.send(longCmd5)
  protected final def longSend6  = cl.send(longCmd6)
  protected final def longSend7  = cl.send(longCmd7)
  protected final def longSend8  = cl.send(longCmd8)
  protected final def longSend9  = cl.send(longCmd9)
  protected final def longSend10 = cl.send(longCmd10)
  protected final def longSend11 = cl.send(longCmd11)
  protected final def longSend12 = cl.send(longCmd12)
  protected final def longSend13 = cl.send(longCmd13)
  protected final def longSend14 = cl.send(longCmd14)
  protected final def longSend15 = cl.send(longCmd15)
  protected final def longSend16 = cl.send(longCmd16)
  protected final def longSend17 = cl.send(longCmd17)
  protected final def longSend18 = cl.send(longCmd18)
  protected final def longSend19 = cl.send(longCmd19)
  protected final def longSend20 = cl.send(longCmd20)
  protected final def longSend21 = cl.send(longCmd21)
  protected final def longSend22 = cl.send(longCmd22)
  protected final def longSend23 = cl.send(longCmd23)
  protected final def longSend24 = cl.send(longCmd24)
  protected final def longSend25 = cl.send(longCmd25)
  protected final def longSend26 = cl.send(longCmd26)
  protected final def longSend27 = cl.send(longCmd27)
  protected final def longSend28 = cl.send(longCmd28)
  protected final def longSend29 = cl.send(longCmd29)
  protected final def longSend30 = cl.send(longCmd30)
  protected final def longSend31 = cl.send(longCmd31)
  protected final def longSend32 = cl.send(longCmd32)
  protected final def longSend33 = cl.send(longCmd33)
  protected final def longSend34 = cl.send(longCmd34)
  protected final def longSend35 = cl.send(longCmd35)
  protected final def longSend36 = cl.send(longCmd36)
  protected final def longSend37 = cl.send(longCmd37)
  protected final def longSend38 = cl.send(longCmd38)
  protected final def longSend39 = cl.send(longCmd39)
  protected final def longSend40 = cl.send(longCmd40)
  protected final def longSend41 = cl.send(longTrim1)
  protected final def longSend42 = cl.send(longTrim2)
  protected final def longSend43 = cl.send(longTrim3)
  protected final def longSend44 = cl.send(longTrim4)
  protected final def longSend45 = cl.send(longTrim5)
  protected final def longSend46 = cl.send(longTrim6)
  protected final def longSend47 = cl.send(longTrim7)
  protected final def longSend48 = cl.send(longTrim8)

  protected final def shortSend1  = cl.send(shortCmd1)
  protected final def shortSend2  = cl.send(shortCmd2)
  protected final def shortSend3  = cl.send(shortCmd3)
  protected final def shortSend4  = cl.send(shortCmd4)
  protected final def shortSend5  = cl.send(shortCmd5)
  protected final def shortSend6  = cl.send(shortCmd6)
  protected final def shortSend7  = cl.send(shortCmd7)
  protected final def shortSend8  = cl.send(shortCmd8)
  protected final def shortSend9  = cl.send(shortCmd9)
  protected final def shortSend10 = cl.send(shortCmd10)
  protected final def shortSend11 = cl.send(shortCmd11)
  protected final def shortSend12 = cl.send(shortCmd12)
  protected final def shortSend13 = cl.send(shortCmd13)
  protected final def shortSend14 = cl.send(shortCmd14)
  protected final def shortSend15 = cl.send(shortCmd15)
  protected final def shortSend16 = cl.send(shortCmd16)
  protected final def shortSend17 = cl.send(shortCmd17)
  protected final def shortSend18 = cl.send(shortCmd18)
  protected final def shortSend19 = cl.send(shortCmd19)
  protected final def shortSend20 = cl.send(shortCmd20)
  protected final def shortSend21 = cl.send(shortCmd21)
  protected final def shortSend22 = cl.send(shortCmd22)
  protected final def shortSend23 = cl.send(shortCmd23)
  protected final def shortSend24 = cl.send(shortCmd24)
  protected final def shortSend25 = cl.send(shortCmd25)
  protected final def shortSend26 = cl.send(shortCmd26)
  protected final def shortSend27 = cl.send(shortCmd27)
  protected final def shortSend28 = cl.send(shortCmd28)
  protected final def shortSend29 = cl.send(shortCmd29)
  protected final def shortSend30 = cl.send(shortCmd30)
  protected final def shortSend31 = cl.send(shortCmd31)
  protected final def shortSend32 = cl.send(shortCmd32)
  protected final def shortSend33 = cl.send(shortCmd33)
  protected final def shortSend34 = cl.send(shortCmd34)
  protected final def shortSend35 = cl.send(shortCmd35)
  protected final def shortSend36 = cl.send(shortCmd36)
  protected final def shortSend37 = cl.send(shortCmd37)
  protected final def shortSend38 = cl.send(shortCmd38)
  protected final def shortSend39 = cl.send(shortCmd39)
  protected final def shortSend40 = cl.send(shortCmd40)
  protected final def shortSend41 = cl.send(shortTrim1)
  protected final def shortSend42 = cl.send(shortTrim2)
  protected final def shortSend43 = cl.send(shortTrim3)
  protected final def shortSend44 = cl.send(shortTrim4)
  protected final def shortSend45 = cl.send(shortTrim5)
  protected final def shortSend46 = cl.send(shortTrim6)
  protected final def shortSend47 = cl.send(shortTrim7)
  protected final def shortSend48 = cl.send(shortTrim8)

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
