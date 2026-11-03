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

/** How a case class field is read from a reply made of fields, by its name: an `Option` field that's missing is `None` */
sealed trait FieldRead[A] {
  def read(name: String, fields: Map[String, Bulk]): Either[RESPDecErr, A]
}

object FieldRead extends FieldReadLowPriority {
  implicit final def optionFieldRead[A](implicit R: Read[Bulk, A]): FieldRead[Option[A]] = new FieldRead[Option[A]] {
    override final def read(name: String, fields: Map[String, Bulk]): Either[RESPDecErr, Option[A]] = fields.get(name) match {
      case Some(bulk) => FieldRead.read(name, bulk)(R).map(Some(_))
      case None       => Right(None)
    }
  }

  private[protocol] final def read[A](name: String, bulk: Bulk)(R: Read[Bulk, A]): Either[RESPDecErr, A] =
    R.read(bulk).left.map(e => RESPDecErr(s"field $name: ${e.message}"))
}

sealed trait FieldReadLowPriority {
  implicit final def fieldRead[A](implicit R: Read[Bulk, A]): FieldRead[A] = new FieldRead[A] {
    override final def read(name: String, fields: Map[String, Bulk]): Either[RESPDecErr, A] = fields.get(name) match {
      case Some(bulk) => FieldRead.read(name, bulk)(R)
      case None       => Left(RESPDecErr(s"missing field $name"))
    }
  }
}

/** How a case class field is written: its name, then its value, or nothing at all for an `Option` field that's `None` */
sealed trait FieldWrite[A] {
  def write(name: String, a: A): Seq[GenBulk]
}

object FieldWrite extends FieldWriteLowPriority {
  implicit final def optionFieldWrite[A](implicit W: RESPParamWrite[A]): FieldWrite[Option[A]] = new FieldWrite[Option[A]] {
    override final def write(name: String, a: Option[A]): Seq[GenBulk] = a.fold(Seq.empty[GenBulk])(Bulk(name) +: W.write(_))
  }
}

sealed trait FieldWriteLowPriority {
  implicit final def fieldWrite[A](implicit W: RESPParamWrite[A]): FieldWrite[A] = new FieldWrite[A] {
    override final def write(name: String, a: A): Seq[GenBulk] = Bulk(name) +: W.write(a)
  }
}

/** Reads a reply made of fields as a case class, by the names of its fields, whatever order they come in; other fields are left out */
private[protocol] object ProductRead {
  private final val fields = Read[Arr.Pairs[Bulk, Bulk], Map[String, Bulk]]

  final def apply[P](read: Map[String, Bulk] => Either[RESPDecErr, P]): Read[Arr.Pairs[Bulk, Bulk], P] =
    Read.instance(pairs => fields.read(pairs).flatMap(read))
}
