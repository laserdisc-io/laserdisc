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
package fs2

import munit.FunSuite

final class Fs2ProtocolHandlerSpec extends FunSuite {
  test("The fs2 protocol handler can be inferred correctly") {
    import cats.effect.IO
    import shapeless.{::, HNil}
    import shapeless.ops.hlist.{Mapper, ZipConst}

    implicitly[
      ZipConst.Aux[
        Env[IO],
        Protocol.Aux[OK] :: HNil,
        (Protocol.Aux[OK], Env[IO]) :: HNil
      ]
    ]

    implicitly[
      Mapper.Aux[
        PromiseMapper.type,
        (Protocol.Aux[OK], Env[IO]) :: HNil,
        IO[Maybe[OK]] :: HNil
      ]
    ]

    implicitly[
      RedisHandler.Aux[
        IO,
        Protocol.Aux[OK] :: HNil,
        Maybe[OK] :: HNil
      ]
    ]

    implicitly[
      Handler.Aux[
        IO,
        Env[IO],
        Protocol.Aux[OK] :: HNil,
        Maybe[OK] :: HNil
      ]
    ]

    implicitly[
      Mapper.Aux[
        PromiseMapper.type,
        (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          HNil,
        IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          HNil
      ]
    ]

    implicitly[
      ZipConst.Aux[
        Env[IO],
        Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          HNil,
        (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          HNil
      ]
    ]

    implicitly[
      Handler.Aux[
        IO,
        Env[IO],
        Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          HNil,
        Maybe[OK] ::
          Maybe[OK] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          HNil
      ]
    ]

    implicitly[
      Mapper.Aux[
        PromiseMapper.type,
        (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[OK], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          (Protocol.Aux[Option[PosInt]], Env[IO]) ::
          HNil,
        IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[OK]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          IO[Maybe[Option[PosInt]]] ::
          HNil
      ]
    ]

    implicitly[
      Handler.Aux[
        IO,
        Env[IO],
        Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[OK] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          Protocol.Aux[Option[PosInt]] ::
          HNil,
        Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[OK] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          Maybe[Option[PosInt]] ::
          HNil
      ]
    ]
  }
}
