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

import laserdisc.redis_8_10.{lrange, ltrim, mset, rpush, set}
import laserdisc.auto.*
import laserdisc.protocol.RESP
import scodec.Codec
import scodec.bits.BitVector

private[fs2] trait TestCommandsProtocol extends TestValues {

  protected final val longCmd1: Protocol.Single[PosInt]     = rpush("key11", longNel1)
  protected final val longCmd2: Protocol.Single[OK]         = set("key12", longString1)
  protected final val longCmd3: Protocol.Single[OK]         = set("key13", 17)
  protected final val longCmd4: Protocol.Single[OK]         = mset(longMulti1)
  protected final val longCmd5: Protocol.Single[Seq[Bulk]]  = lrange("key11", 0L, 100L)
  protected final val longCmd6: Protocol.Single[OK]         = set("key21", longString2)
  protected final val longCmd7: Protocol.Single[PosInt]     = rpush("key22", longNel2)
  protected final val longCmd8: Protocol.Single[Seq[Bulk]]  = lrange("key22", 0L, 100L)
  protected final val longCmd9: Protocol.Single[OK]         = set("key23", 17)
  protected final val longCmd10: Protocol.Single[OK]        = mset(longMulti2)
  protected final val longCmd11: Protocol.Single[PosInt]    = rpush("key31", longNel3)
  protected final val longCmd12: Protocol.Single[OK]        = set("key32", longString3)
  protected final val longCmd13: Protocol.Single[OK]        = mset(longMulti3)
  protected final val longCmd14: Protocol.Single[Seq[Bulk]] = lrange("key31", 0L, 100L)
  protected final val longCmd15: Protocol.Single[OK]        = set("key33", 17)
  protected final val longCmd16: Protocol.Single[OK]        = set("key41", longString4)
  protected final val longCmd17: Protocol.Single[OK]        = set("key42", 17)
  protected final val longCmd18: Protocol.Single[PosInt]    = rpush("key43", longNel4)
  protected final val longCmd19: Protocol.Single[Seq[Bulk]] = lrange("key43", 0L, 100L)
  protected final val longCmd20: Protocol.Single[OK]        = mset(longMulti4)
  protected final val longCmd21: Protocol.Single[PosInt]    = rpush("key51", longNel5)
  protected final val longCmd22: Protocol.Single[OK]        = set("key52", longString5)
  protected final val longCmd23: Protocol.Single[OK]        = set("key53", 17)
  protected final val longCmd24: Protocol.Single[OK]        = mset(longMulti5)
  protected final val longCmd25: Protocol.Single[Seq[Bulk]] = lrange("key51", 0L, 100L)
  protected final val longCmd26: Protocol.Single[OK]        = set("key61", longString6)
  protected final val longCmd27: Protocol.Single[PosInt]    = rpush("key62", longNel6)
  protected final val longCmd28: Protocol.Single[Seq[Bulk]] = lrange("key62", 0L, 100L)
  protected final val longCmd29: Protocol.Single[OK]        = set("key63", 17)
  protected final val longCmd30: Protocol.Single[OK]        = mset(longMulti6)
  protected final val longCmd31: Protocol.Single[OK]        = set("key71", longString7)
  protected final val longCmd32: Protocol.Single[OK]        = set("key72", 17)
  protected final val longCmd33: Protocol.Single[PosInt]    = rpush("key73", longNel7)
  protected final val longCmd34: Protocol.Single[OK]        = mset(longMulti7)
  protected final val longCmd35: Protocol.Single[Seq[Bulk]] = lrange("key73", 0L, 100L)
  protected final val longCmd36: Protocol.Single[OK]        = set("key81", longString8)
  protected final val longCmd37: Protocol.Single[OK]        = set("key82", 17)
  protected final val longCmd38: Protocol.Single[PosInt]    = rpush("key83", longNel8)
  protected final val longCmd39: Protocol.Single[Seq[Bulk]] = lrange("key83", 0L, 100L)
  protected final val longCmd40: Protocol.Single[OK]        = mset(longMulti8)

  protected final val shortCmd1: Protocol.Single[PosInt]     = rpush("key11", shortNel1)
  protected final val shortCmd2: Protocol.Single[OK]         = set("key12", shortString1)
  protected final val shortCmd3: Protocol.Single[OK]         = set("key13", 17)
  protected final val shortCmd4: Protocol.Single[OK]         = mset(shortMulti1)
  protected final val shortCmd5: Protocol.Single[Seq[Bulk]]  = lrange("key11", 0L, 4L)
  protected final val shortCmd6: Protocol.Single[OK]         = set("key21", shortString2)
  protected final val shortCmd7: Protocol.Single[PosInt]     = rpush("key22", shortNel2)
  protected final val shortCmd8: Protocol.Single[Seq[Bulk]]  = lrange("key22", 0L, 4L)
  protected final val shortCmd9: Protocol.Single[OK]         = set("key23", 17)
  protected final val shortCmd10: Protocol.Single[OK]        = mset(shortMulti2)
  protected final val shortCmd11: Protocol.Single[PosInt]    = rpush("key31", shortNel3)
  protected final val shortCmd12: Protocol.Single[OK]        = set("key32", shortString3)
  protected final val shortCmd13: Protocol.Single[OK]        = mset(shortMulti3)
  protected final val shortCmd14: Protocol.Single[Seq[Bulk]] = lrange("key31", 0L, 4L)
  protected final val shortCmd15: Protocol.Single[OK]        = set("key33", 17)
  protected final val shortCmd16: Protocol.Single[OK]        = set("key41", shortString4)
  protected final val shortCmd17: Protocol.Single[OK]        = set("key42", 17)
  protected final val shortCmd18: Protocol.Single[PosInt]    = rpush("key43", shortNel4)
  protected final val shortCmd19: Protocol.Single[Seq[Bulk]] = lrange("key43", 0L, 4L)
  protected final val shortCmd20: Protocol.Single[OK]        = mset(shortMulti4)
  protected final val shortCmd21: Protocol.Single[PosInt]    = rpush("key51", shortNel5)
  protected final val shortCmd22: Protocol.Single[OK]        = set("key52", shortString5)
  protected final val shortCmd23: Protocol.Single[OK]        = set("key53", 17)
  protected final val shortCmd24: Protocol.Single[OK]        = mset(shortMulti5)
  protected final val shortCmd25: Protocol.Single[Seq[Bulk]] = lrange("key51", 0L, 4L)
  protected final val shortCmd26: Protocol.Single[OK]        = set("key61", shortString6)
  protected final val shortCmd27: Protocol.Single[PosInt]    = rpush("key62", shortNel6)
  protected final val shortCmd28: Protocol.Single[Seq[Bulk]] = lrange("key62", 0L, 4L)
  protected final val shortCmd29: Protocol.Single[OK]        = set("key63", 17)
  protected final val shortCmd30: Protocol.Single[OK]        = mset(shortMulti6)
  protected final val shortCmd31: Protocol.Single[OK]        = set("key71", shortString7)
  protected final val shortCmd32: Protocol.Single[OK]        = set("key72", 17)
  protected final val shortCmd33: Protocol.Single[PosInt]    = rpush("key73", shortNel7)
  protected final val shortCmd34: Protocol.Single[OK]        = mset(shortMulti7)
  protected final val shortCmd35: Protocol.Single[Seq[Bulk]] = lrange("key73", 0L, 4L)
  protected final val shortCmd36: Protocol.Single[OK]        = set("key81", shortString8)
  protected final val shortCmd37: Protocol.Single[OK]        = set("key82", 17)
  protected final val shortCmd38: Protocol.Single[PosInt]    = rpush("key83", shortNel8)
  protected final val shortCmd39: Protocol.Single[Seq[Bulk]] = lrange("key83", 0L, 4L)
  protected final val shortCmd40: Protocol.Single[OK]        = mset(shortMulti8)

  protected final val longTrim1: Protocol.Single[OK] = ltrim("key11", Index(0L), Index(200L))
  protected final val longTrim2: Protocol.Single[OK] = ltrim("key22", Index(0L), Index(200L))
  protected final val longTrim3: Protocol.Single[OK] = ltrim("key31", Index(0L), Index(200L))
  protected final val longTrim4: Protocol.Single[OK] = ltrim("key43", Index(0L), Index(200L))
  protected final val longTrim5: Protocol.Single[OK] = ltrim("key51", Index(0L), Index(200L))
  protected final val longTrim6: Protocol.Single[OK] = ltrim("key62", Index(0L), Index(200L))
  protected final val longTrim7: Protocol.Single[OK] = ltrim("key73", Index(0L), Index(200L))
  protected final val longTrim8: Protocol.Single[OK] = ltrim("key83", Index(0L), Index(200L))

  protected final val shortTrim1: Protocol.Single[OK] = ltrim("key11", Index(0L), Index(5L))
  protected final val shortTrim2: Protocol.Single[OK] = ltrim("key22", Index(0L), Index(5L))
  protected final val shortTrim3: Protocol.Single[OK] = ltrim("key31", Index(0L), Index(5L))
  protected final val shortTrim4: Protocol.Single[OK] = ltrim("key43", Index(0L), Index(5L))
  protected final val shortTrim5: Protocol.Single[OK] = ltrim("key51", Index(0L), Index(5L))
  protected final val shortTrim6: Protocol.Single[OK] = ltrim("key62", Index(0L), Index(5L))
  protected final val shortTrim7: Protocol.Single[OK] = ltrim("key73", Index(0L), Index(5L))
  protected final val shortTrim8: Protocol.Single[OK] = ltrim("key83", Index(0L), Index(5L))
}

private[fs2] trait TestCommandsResp extends TestCommandsProtocol {
  protected val longResp1  = longCmd1.encode
  protected val longResp2  = longCmd2.encode
  protected val longResp3  = longCmd3.encode
  protected val longResp4  = longCmd4.encode
  protected val longResp5  = longCmd5.encode
  protected val longResp6  = longCmd6.encode
  protected val longResp7  = longCmd7.encode
  protected val longResp8  = longCmd8.encode
  protected val longResp9  = longCmd9.encode
  protected val longResp10 = longCmd10.encode
  protected val longResp11 = longCmd11.encode
  protected val longResp12 = longCmd12.encode
  protected val longResp13 = longCmd13.encode
  protected val longResp14 = longCmd14.encode
  protected val longResp15 = longCmd15.encode
  protected val longResp16 = longCmd16.encode
  protected val longResp17 = longCmd17.encode
  protected val longResp18 = longCmd18.encode
  protected val longResp19 = longCmd19.encode
  protected val longResp20 = longCmd20.encode
  protected val longResp21 = longCmd21.encode
  protected val longResp22 = longCmd22.encode
  protected val longResp23 = longCmd23.encode
  protected val longResp24 = longCmd24.encode
  protected val longResp25 = longCmd25.encode
  protected val longResp26 = longCmd26.encode
  protected val longResp27 = longCmd27.encode
  protected val longResp28 = longCmd28.encode
  protected val longResp29 = longCmd29.encode
  protected val longResp30 = longCmd30.encode
  protected val longResp31 = longCmd31.encode
  protected val longResp32 = longCmd32.encode
  protected val longResp33 = longCmd33.encode
  protected val longResp34 = longCmd34.encode
  protected val longResp35 = longCmd35.encode
  protected val longResp36 = longCmd36.encode
  protected val longResp37 = longCmd37.encode
  protected val longResp38 = longCmd38.encode
  protected val longResp39 = longCmd39.encode
  protected val longResp40 = longCmd40.encode
  protected val longResp41 = longTrim1.encode
  protected val longResp42 = longTrim2.encode
  protected val longResp43 = longTrim3.encode
  protected val longResp44 = longTrim4.encode
  protected val longResp45 = longTrim5.encode
  protected val longResp46 = longTrim6.encode
  protected val longResp47 = longTrim7.encode
  protected val longResp48 = longTrim8.encode

  protected val shortResp1  = shortCmd1.encode
  protected val shortResp2  = shortCmd2.encode
  protected val shortResp3  = shortCmd3.encode
  protected val shortResp4  = shortCmd4.encode
  protected val shortResp5  = shortCmd5.encode
  protected val shortResp6  = shortCmd6.encode
  protected val shortResp7  = shortCmd7.encode
  protected val shortResp8  = shortCmd8.encode
  protected val shortResp9  = shortCmd9.encode
  protected val shortResp10 = shortCmd10.encode
  protected val shortResp11 = shortCmd11.encode
  protected val shortResp12 = shortCmd12.encode
  protected val shortResp13 = shortCmd13.encode
  protected val shortResp14 = shortCmd14.encode
  protected val shortResp15 = shortCmd15.encode
  protected val shortResp16 = shortCmd16.encode
  protected val shortResp17 = shortCmd17.encode
  protected val shortResp18 = shortCmd18.encode
  protected val shortResp19 = shortCmd19.encode
  protected val shortResp20 = shortCmd20.encode
  protected val shortResp21 = shortCmd21.encode
  protected val shortResp22 = shortCmd22.encode
  protected val shortResp23 = shortCmd23.encode
  protected val shortResp24 = shortCmd24.encode
  protected val shortResp25 = shortCmd25.encode
  protected val shortResp26 = shortCmd26.encode
  protected val shortResp27 = shortCmd27.encode
  protected val shortResp28 = shortCmd28.encode
  protected val shortResp29 = shortCmd29.encode
  protected val shortResp30 = shortCmd30.encode
  protected val shortResp31 = shortCmd31.encode
  protected val shortResp32 = shortCmd32.encode
  protected val shortResp33 = shortCmd33.encode
  protected val shortResp34 = shortCmd34.encode
  protected val shortResp35 = shortCmd35.encode
  protected val shortResp36 = shortCmd36.encode
  protected val shortResp37 = shortCmd37.encode
  protected val shortResp38 = shortCmd38.encode
  protected val shortResp39 = shortCmd39.encode
  protected val shortResp40 = shortCmd40.encode
  protected val shortResp41 = shortTrim1.encode
  protected val shortResp42 = shortTrim2.encode
  protected val shortResp43 = shortTrim3.encode
  protected val shortResp44 = shortTrim4.encode
  protected val shortResp45 = shortTrim5.encode
  protected val shortResp46 = shortTrim6.encode
  protected val shortResp47 = shortTrim7.encode
  protected val shortResp48 = shortTrim8.encode
}

private[fs2] trait TestCommandsBitVector extends TestCommandsResp {

  private val respEncoder = Codec[RESP]

  protected val longBitVector1: BitVector  = respEncoder.encode(longResp1).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector2: BitVector  = respEncoder.encode(longResp2).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector3: BitVector  = respEncoder.encode(longResp3).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector4: BitVector  = respEncoder.encode(longResp4).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector5: BitVector  = respEncoder.encode(longResp5).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector6: BitVector  = respEncoder.encode(longResp6).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector7: BitVector  = respEncoder.encode(longResp7).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector8: BitVector  = respEncoder.encode(longResp8).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector9: BitVector  = respEncoder.encode(longResp9).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector10: BitVector = respEncoder.encode(longResp10).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector11: BitVector = respEncoder.encode(longResp11).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector12: BitVector = respEncoder.encode(longResp12).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector13: BitVector = respEncoder.encode(longResp13).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector14: BitVector = respEncoder.encode(longResp14).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector15: BitVector = respEncoder.encode(longResp15).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector16: BitVector = respEncoder.encode(longResp16).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector17: BitVector = respEncoder.encode(longResp17).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector18: BitVector = respEncoder.encode(longResp18).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector19: BitVector = respEncoder.encode(longResp19).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector20: BitVector = respEncoder.encode(longResp20).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector21: BitVector = respEncoder.encode(longResp21).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector22: BitVector = respEncoder.encode(longResp22).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector23: BitVector = respEncoder.encode(longResp23).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector24: BitVector = respEncoder.encode(longResp24).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector25: BitVector = respEncoder.encode(longResp25).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector26: BitVector = respEncoder.encode(longResp26).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector27: BitVector = respEncoder.encode(longResp27).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector28: BitVector = respEncoder.encode(longResp28).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector29: BitVector = respEncoder.encode(longResp29).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector30: BitVector = respEncoder.encode(longResp30).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector31: BitVector = respEncoder.encode(longResp31).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector32: BitVector = respEncoder.encode(longResp32).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector33: BitVector = respEncoder.encode(longResp33).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector34: BitVector = respEncoder.encode(longResp34).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector35: BitVector = respEncoder.encode(longResp35).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector36: BitVector = respEncoder.encode(longResp36).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector37: BitVector = respEncoder.encode(longResp37).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector38: BitVector = respEncoder.encode(longResp38).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector39: BitVector = respEncoder.encode(longResp39).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector40: BitVector = respEncoder.encode(longResp40).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector41: BitVector = respEncoder.encode(longResp41).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector42: BitVector = respEncoder.encode(longResp42).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector43: BitVector = respEncoder.encode(longResp43).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector44: BitVector = respEncoder.encode(longResp44).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector45: BitVector = respEncoder.encode(longResp45).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector46: BitVector = respEncoder.encode(longResp46).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector47: BitVector = respEncoder.encode(longResp47).getOrElse(throw new Exception("failed encode"))
  protected val longBitVector48: BitVector = respEncoder.encode(longResp48).getOrElse(throw new Exception("failed encode"))

  protected val shortBitVector1: BitVector  = respEncoder.encode(shortResp1).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector2: BitVector  = respEncoder.encode(shortResp2).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector3: BitVector  = respEncoder.encode(shortResp3).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector4: BitVector  = respEncoder.encode(shortResp4).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector5: BitVector  = respEncoder.encode(shortResp5).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector6: BitVector  = respEncoder.encode(shortResp6).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector7: BitVector  = respEncoder.encode(shortResp7).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector8: BitVector  = respEncoder.encode(shortResp8).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector9: BitVector  = respEncoder.encode(shortResp9).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector10: BitVector = respEncoder.encode(shortResp10).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector11: BitVector = respEncoder.encode(shortResp11).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector12: BitVector = respEncoder.encode(shortResp12).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector13: BitVector = respEncoder.encode(shortResp13).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector14: BitVector = respEncoder.encode(shortResp14).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector15: BitVector = respEncoder.encode(shortResp15).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector16: BitVector = respEncoder.encode(shortResp16).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector17: BitVector = respEncoder.encode(shortResp17).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector18: BitVector = respEncoder.encode(shortResp18).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector19: BitVector = respEncoder.encode(shortResp19).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector20: BitVector = respEncoder.encode(shortResp20).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector21: BitVector = respEncoder.encode(shortResp21).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector22: BitVector = respEncoder.encode(shortResp22).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector23: BitVector = respEncoder.encode(shortResp23).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector24: BitVector = respEncoder.encode(shortResp24).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector25: BitVector = respEncoder.encode(shortResp25).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector26: BitVector = respEncoder.encode(shortResp26).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector27: BitVector = respEncoder.encode(shortResp27).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector28: BitVector = respEncoder.encode(shortResp28).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector29: BitVector = respEncoder.encode(shortResp29).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector30: BitVector = respEncoder.encode(shortResp30).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector31: BitVector = respEncoder.encode(shortResp31).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector32: BitVector = respEncoder.encode(shortResp32).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector33: BitVector = respEncoder.encode(shortResp33).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector34: BitVector = respEncoder.encode(shortResp34).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector35: BitVector = respEncoder.encode(shortResp35).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector36: BitVector = respEncoder.encode(shortResp36).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector37: BitVector = respEncoder.encode(shortResp37).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector38: BitVector = respEncoder.encode(shortResp38).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector39: BitVector = respEncoder.encode(shortResp39).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector40: BitVector = respEncoder.encode(shortResp40).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector41: BitVector = respEncoder.encode(shortResp41).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector42: BitVector = respEncoder.encode(shortResp42).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector43: BitVector = respEncoder.encode(shortResp43).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector44: BitVector = respEncoder.encode(shortResp44).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector45: BitVector = respEncoder.encode(shortResp45).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector46: BitVector = respEncoder.encode(shortResp46).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector47: BitVector = respEncoder.encode(shortResp47).getOrElse(throw new Exception("failed encode"))
  protected val shortBitVector48: BitVector = respEncoder.encode(shortResp48).getOrElse(throw new Exception("failed encode"))
}
