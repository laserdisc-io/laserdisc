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

package object protocol {
  // a SCAN reply: the cursor, then the values, if any, as `E`
  private[protocol] final type ScanOf[E] = Arr.Tuple[Bulk *: OrNilArr[E] *: EmptyTuple]

  // a SCAN reply's cursor and values, for Loop.cursor
  private[protocol] final def scanned[A](scan: Scan[A]): (NonNegLong, Seq[A])        = scan.cursor -> scan.values.getOrElse(Nil)
  private[protocol] final def scannedKV(scan: ScanKV): (NonNegLong, Seq[KV[String]]) = scan.cursor -> scan.maybeValues.getOrElse(Nil)

  // the key ends at the first colon, so values may have colons
  private[protocol] final val KVPairRegex = "([^:]*):(.*)".r

  private[protocol] val KVP: Read[String, (String, String)] = {
    case KVPairRegex(k, v) => Right(k -> v)
    case other => Left(RESPDecErr(s"Read[String, (String, String)], error decoding key -> value pair. Expected [key:value] but was $other"))
  }

  private[protocol] val KVPS: Read[Seq[String], List[(String, String)]] = Read.instance { ss =>
    ss.foldRight[Either[RESPDecErr, (List[(String, String)], Int)]](Right(Nil -> 0)) {
      case (KVP(Right((k, v))), Right((kvs, kvl))) => Right(((k -> v) :: kvs) -> (kvl + 1))
      case (KVP(Left(e)), Right((_, kvl)))         =>
        Left(
          RESPDecErr(s"Read[List[String], List[(String, String)]], Error decoding key value pairs at position ${kvl + 1}. Error was: $e")
        )
      case (_, left) => left
    }.map(_._1)
  }
}
