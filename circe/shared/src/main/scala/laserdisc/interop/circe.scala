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
package interop

import io.circe.*
import cats.syntax.either.*
import cats.syntax.show.*
import io.circe.syntax.*

/** JSON with circe: an `io.circe.Json`, as made by `asJson`, is written as compact JSON and read back. Other values, such as a `String`,
  * are written and read as they are. A type with an `Encoder` and a `Decoder` can also be written and read as JSON with [[showAsJson]] and
  * [[readAsJson]]
  */
object circe {
  implicit final val circeJsonShow: Show[Json]       = Show.instance(_.noSpaces)
  implicit final val circeJsonRead: Read[Bulk, Json] =
    Read.instance { case Bulk(s) =>
      parser.parse(s).leftMap(err => RESPDecErr(err.show))
    }

  /** A `Show` writing `A` as JSON, for a type chosen to be written that way */
  final def showAsJson[A: Encoder]: Show[A] = circeJsonShow.contramap(_.asJson)

  /** A `Read` reading `A` from JSON, for a type chosen to be read that way */
  final def readAsJson[A: Decoder]: Read[Bulk, A] =
    Read.instance { case Bulk(s) =>
      parser.decode[A](s).leftMap(err => RESPDecErr(err.show))
    }
}
