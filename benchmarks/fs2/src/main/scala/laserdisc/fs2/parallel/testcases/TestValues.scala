package laserdisc
package fs2
package parallel
package testcases

import java.util.UUID

import laserdisc.auto._

private[fs2] trait TestValues {
  private[this] def generateList(n: Int): OneOrMore[String] =
    OneOrMore.unsafeFrom((1 to n).map(_ => UUID.randomUUID().toString).toList)

  private[this] def generateString(n: Int): String =
    (1 to n).map(_ => UUID.randomUUID().toString).mkString(" ")

  private[this] def generateMulti(n: Int): OneOrMore[(Key, Int)] =
    OneOrMore.unsafeFrom(
      (1 to n).map(c => (Key.unsafeFrom(s"keyM$c"), UUID.randomUUID().hashCode())).toList
    )

  private[fs2] val longNel1 = generateList(200)
  private[fs2] val longNel2 = generateList(100)
  private[fs2] val longNel3 = generateList(200)
  private[fs2] val longNel4 = generateList(100)
  private[fs2] val longNel5 = generateList(200)
  private[fs2] val longNel6 = generateList(100)
  private[fs2] val longNel7 = generateList(200)
  private[fs2] val longNel8 = generateList(100)

  private[fs2] val longList1 = longNel1.value
  private[fs2] val longList2 = longNel2.value
  private[fs2] val longList3 = longNel3.value
  private[fs2] val longList4 = longNel4.value
  private[fs2] val longList5 = longNel5.value
  private[fs2] val longList6 = longNel6.value
  private[fs2] val longList7 = longNel7.value
  private[fs2] val longList8 = longNel8.value

  private[fs2] val longString1 = generateString(50)
  private[fs2] val longString2 = generateString(40)
  private[fs2] val longString3 = generateString(50)
  private[fs2] val longString4 = generateString(40)
  private[fs2] val longString5 = generateString(50)
  private[fs2] val longString6 = generateString(40)
  private[fs2] val longString7 = generateString(50)
  private[fs2] val longString8 = generateString(40)

  private[fs2] val longMulti1 = generateMulti(20)
  private[fs2] val longMulti2 = generateMulti(10)
  private[fs2] val longMulti3 = generateMulti(20)
  private[fs2] val longMulti4 = generateMulti(10)
  private[fs2] val longMulti5 = generateMulti(20)
  private[fs2] val longMulti6 = generateMulti(10)
  private[fs2] val longMulti7 = generateMulti(20)
  private[fs2] val longMulti8 = generateMulti(10)

  private[fs2] val longMulti1Map = longMulti1.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val longMulti2Map = longMulti2.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val longMulti3Map = longMulti3.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val longMulti4Map = longMulti4.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val longMulti5Map = longMulti5.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val longMulti6Map = longMulti6.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val longMulti7Map = longMulti7.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val longMulti8Map = longMulti8.map { case (k, v) => k.value -> v }.toMap

  private[fs2] val longMulti1MapStr = longMulti1.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val longMulti2MapStr = longMulti2.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val longMulti3MapStr = longMulti3.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val longMulti4MapStr = longMulti4.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val longMulti5MapStr = longMulti5.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val longMulti6MapStr = longMulti6.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val longMulti7MapStr = longMulti7.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val longMulti8MapStr = longMulti8.map { case (k, v) => k.value -> v.toString }.toMap

  private[fs2] val shortNel1 = generateList(5)
  private[fs2] val shortNel2 = generateList(4)
  private[fs2] val shortNel3 = generateList(5)
  private[fs2] val shortNel4 = generateList(4)
  private[fs2] val shortNel5 = generateList(5)
  private[fs2] val shortNel6 = generateList(4)
  private[fs2] val shortNel7 = generateList(5)
  private[fs2] val shortNel8 = generateList(4)

  private[fs2] val shortList1 = shortNel1.value
  private[fs2] val shortList2 = shortNel2.value
  private[fs2] val shortList3 = shortNel3.value
  private[fs2] val shortList4 = shortNel4.value
  private[fs2] val shortList5 = shortNel5.value
  private[fs2] val shortList6 = shortNel6.value
  private[fs2] val shortList7 = shortNel7.value
  private[fs2] val shortList8 = shortNel8.value

  private[fs2] val shortString1 = generateString(5)
  private[fs2] val shortString2 = generateString(4)
  private[fs2] val shortString3 = generateString(5)
  private[fs2] val shortString4 = generateString(4)
  private[fs2] val shortString5 = generateString(5)
  private[fs2] val shortString6 = generateString(4)
  private[fs2] val shortString7 = generateString(5)
  private[fs2] val shortString8 = generateString(4)

  private[fs2] val shortMulti1 = generateMulti(3)
  private[fs2] val shortMulti2 = generateMulti(4)
  private[fs2] val shortMulti3 = generateMulti(3)
  private[fs2] val shortMulti4 = generateMulti(4)
  private[fs2] val shortMulti5 = generateMulti(3)
  private[fs2] val shortMulti6 = generateMulti(4)
  private[fs2] val shortMulti7 = generateMulti(3)
  private[fs2] val shortMulti8 = generateMulti(4)

  private[fs2] val shortMulti1Map = shortMulti1.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val shortMulti2Map = shortMulti2.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val shortMulti3Map = shortMulti3.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val shortMulti4Map = shortMulti4.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val shortMulti5Map = shortMulti5.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val shortMulti6Map = shortMulti6.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val shortMulti7Map = shortMulti7.map { case (k, v) => k.value -> v }.toMap
  private[fs2] val shortMulti8Map = shortMulti8.map { case (k, v) => k.value -> v }.toMap

  private[fs2] val shortMulti1MapStr = shortMulti1.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val shortMulti2MapStr = shortMulti2.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val shortMulti3MapStr = shortMulti3.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val shortMulti4MapStr = shortMulti4.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val shortMulti5MapStr = shortMulti5.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val shortMulti6MapStr = shortMulti6.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val shortMulti7MapStr = shortMulti7.map { case (k, v) => k.value -> v.toString }.toMap
  private[fs2] val shortMulti8MapStr = shortMulti8.map { case (k, v) => k.value -> v.toString }.toMap
}
