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

package elsewhere

// what users write, outside laserdisc's packages, where only what's public is found: the readers and writers that need to tell types apart
final class OutsideTheLibrarySpec extends munit.FunSuite {
  test("a reply that may be nil, in a script, a field or an element, can be read from outside laserdisc") {
    assert(
      compileErrors(
        """laserdisc.redis_8_10.hmget[Int, Option[String]](laserdisc.Key("h"), laserdisc.Key("a"), laserdisc.Key("b"))"""
      ).isEmpty
    )
    assert(
      compileErrors(
        """laserdisc.redis_8_10.eval[laserdisc.OrNullBulk[laserdisc.protocol.Bulk], Option[String]]("return 1", laserdisc.OneOrMoreKeys.unsafeFrom(List(laserdisc.Key("k"))))"""
      ).isEmpty
    )
  }

  test("a case class can be written to a hash and read from one from outside laserdisc") {
    assert(compileErrors("""laserdisc.redis_8_10.hset(laserdisc.Key("u"), User("ada", 36, None))""").isEmpty)
    assert(compileErrors("""laserdisc.redis_8_10.hgetall[User](laserdisc.Key("u"))""").isEmpty)
  }
}

final case class User(name: String, age: Int, email: Option[String])
