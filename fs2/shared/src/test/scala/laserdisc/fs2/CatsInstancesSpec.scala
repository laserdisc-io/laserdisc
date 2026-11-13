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

import cats.effect.IO
import munit.CatsEffectSuite

object CatsInstancesSpec {
  final case class Shown(value: Int)
  object Shown {
    implicit val catsShow: cats.Show[Shown] = cats.Show.show(s => s"shown ${s.value}")
  }
}

final class CatsInstancesSpec extends CatsEffectSuite {
  test("a cats Show doesn't give a Show") {
    assert(compileErrors("implicitly[Show[CatsInstancesSpec.Shown]]").nonEmpty)
  }

  test("a command sends a tuple as its elements, though cats has a Show for the tuple") {
    assertEquals(Protocol("X", "a" *: 1L *: "c" *: EmptyTuple).as[Str, OK].encode, Arr(Bulk("X"), Bulk("a"), Bulk("1"), Bulk("c")))
  }

  test("a cats Functor gives a Functor") {
    functorForCats[IO].map(IO.pure(1))(_ + 1).assertEquals(2)
  }

  test("unfold goes through every step") {
    unfoldForFs2[IO]
      .unfold[Int, Int](1)(n => IO.pure(Right(Seq(n, n * 10) -> (if (n < 2) Some(n + 1) else None))))
      .compile
      .toList
      .assertEquals(List(1, 10, 2, 20))
  }

  test("unfold fails with the error of a failed step") {
    val failure = new RuntimeException("failed")
    unfoldForFs2[IO].unfold[Int, Int](1)(_ => IO.pure(Left(failure))).compile.toList.attempt.assertEquals(Left(failure))
  }
}
