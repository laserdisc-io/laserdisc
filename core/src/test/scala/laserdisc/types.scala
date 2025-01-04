/*
 * Copyright (c) 2018-2025 LaserDisc
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

import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

final case class Foo(x: Int)
object Foo {
  implicit final val fooRead: Bulk ==> Foo = Read.instance {
    case Bulk(ToInt(i)) => Right(Foo(i))
    case Bulk(other)    => Left(RESPDecErr(s"Boom: $other"))
  }
}

final case class Bar(x: String)

final case class Baz(f1: Int, f2: String)
object Baz {
  implicit final val bazArb: Arbitrary[Baz] = Arbitrary {
    for {
      i <- arbitrary[Int]
      s <- arbitrary[String]
    } yield Baz(i, s)
  }
}

private[laserdisc] sealed trait ProtocolEncoded extends Product with Serializable { def encoded: String }
private[laserdisc] final case class ArrEncoded(v: List[ProtocolEncoded]) extends ProtocolEncoded {
  def encoded: String = s"*${v.size}$CRLF${v.map(_.encoded).mkString}"
}
private[laserdisc] final case class EmptyArrEncoded() extends ProtocolEncoded {
  def encoded: String = s"*0$CRLF"
}
private[laserdisc] final case class NullArrEncoded() extends ProtocolEncoded {
  def encoded: String = s"*-1$CRLF"
}
private[laserdisc] final case class EmptyBulkEncoded() extends ProtocolEncoded {
  def encoded: String = s"$$0$CRLF$CRLF"
}
private[laserdisc] final case class BulkEncoded(v: NonEmptyString) extends ProtocolEncoded {
  def encoded: String = s"$$${v.value.getBytes.length}$CRLF${v.value}$CRLF"
}
private[laserdisc] final case class NullBulkEncoded() extends ProtocolEncoded {
  def encoded: String = s"$$-1$CRLF"
}
private[laserdisc] final case class NumEncoded(v: Long) extends ProtocolEncoded {
  def encoded: String = s":$v$CRLF"
}
private[laserdisc] final case class StrEncoded(v: String) extends ProtocolEncoded {
  def encoded: String = s"+$v$CRLF"
}
private[laserdisc] final case class ErrEncoded(v: NonEmptyString) extends ProtocolEncoded {
  def encoded: String = s"-${v.value}$CRLF"
}
