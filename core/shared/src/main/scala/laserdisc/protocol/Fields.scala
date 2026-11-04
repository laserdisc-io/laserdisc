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

/** The fields of a reply made of names and values (a [[Record]]), read one at a time by name, each saying which RESP type it is */
private[protocol] final class Fields(values: Map[String, RESP]) {

  /** A field that's always there. One that may be nil says so with a [[OneOf]], like `f[OrNullBulk[Bulk], Option[String]]("description")`
    */
  def apply[A, B](name: String)(implicit S: RESPSub[A], R: Read[A, B]): Either[RESPDecErr, B] = values.get(name) match {
    case Some(resp) => value[A, B](name, resp)
    case None       => Left(RESPDecErr(s"missing field $name"))
  }

  /** A field that may not be there at all, like one that only newer versions send */
  def optional[A, B](name: String)(implicit S: RESPSub[A], R: Read[A, B]): Either[RESPDecErr, Option[B]] = values.get(name) match {
    case Some(resp) => value[A, B](name, resp).map(Some(_))
    case None       => Right(None)
  }

  def bulk[B](name: String)(implicit R: Read[Bulk, B]): Either[RESPDecErr, B]                 = apply[Bulk, B](name)
  def num[B](name: String)(implicit R: Read[Num, B]): Either[RESPDecErr, B]                   = apply[Num, B](name)
  def optionalBulk[B](name: String)(implicit R: Read[Bulk, B]): Either[RESPDecErr, Option[B]] = optional[Bulk, B](name)
  def optionalNum[B](name: String)(implicit R: Read[Num, B]): Either[RESPDecErr, Option[B]]   = optional[Num, B](name)

  private def value[A, B](name: String, resp: RESP)(implicit S: RESPSub[A], R: Read[A, B]): Either[RESPDecErr, B] = S.unapply(resp) match {
    case Some(a) => R.read(a).left.map(e => RESPDecErr(s"field $name: ${e.message}"))
    case None    => Left(RESPDecErr(s"field $name: Unexpected value. Was $resp"))
  }
}
