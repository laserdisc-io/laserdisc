import munit.FunSuite

final class Fs2ProtocolHandlerSpec extends FunSuite {
  test("The fs2 protocol handler can be inferred correctly") {
    import laserdisc.fs2.PromiseMapper
    import shapeless.::
    import shapeless.ops.hlist.{Mapper, ZipConst}

    implicitly[
      ZipConst.Aux[
        laserdisc.fs2.Env[cats.effect.IO],
        laserdisc.Protocol.Aux[laserdisc.OK] :: shapeless.HNil,
        (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) :: shapeless.HNil
      ]
    ]

    implicitly[
      Mapper.Aux[
        PromiseMapper.type,
        (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) :: shapeless.HNil,
        cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] :: shapeless.HNil,
      ]
    ]

    implicitly[
      laserdisc.fs2.RedisHandler.Aux[
        cats.effect.IO,
        laserdisc.Protocol.Aux[laserdisc.OK] :: shapeless.HNil,
        laserdisc.Maybe[laserdisc.OK] :: shapeless.HNil
      ]
    ]

    implicitly[
      laserdisc.Handler.Aux[
        cats.effect.IO,
        laserdisc.fs2.Env[cats.effect.IO],
        laserdisc.Protocol.Aux[laserdisc.OK] :: shapeless.HNil,
        laserdisc.Maybe[laserdisc.OK] :: shapeless.HNil
      ]
    ]

    implicitly[
      Mapper.Aux[
        PromiseMapper.type,
        (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          shapeless.HNil,
        cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          shapeless.HNil,
      ]
    ]

    implicitly[
      ZipConst.Aux[
        laserdisc.fs2.Env[cats.effect.IO],
        laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          shapeless.HNil,
        (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          shapeless.HNil
      ]
    ]

    implicitly[
      laserdisc.Handler.Aux[
        cats.effect.IO,
        laserdisc.fs2.Env[cats.effect.IO],
        laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          shapeless.HNil,
        laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          shapeless.HNil
      ]
    ]

    implicitly[
      Mapper.Aux[
        PromiseMapper.type,
        (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[laserdisc.OK], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          (laserdisc.Protocol.Aux[Option[laserdisc.PosInt]], laserdisc.fs2.Env[cats.effect.IO]) ::
          shapeless.HNil,
        cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[laserdisc.OK]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          cats.effect.IO[laserdisc.Maybe[Option[laserdisc.PosInt]]] ::
          shapeless.HNil,
      ]
    ]

    implicitly[
      laserdisc.Handler.Aux[
        cats.effect.IO,
        laserdisc.fs2.Env[cats.effect.IO],
        laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[laserdisc.OK] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          laserdisc.Protocol.Aux[Option[laserdisc.PosInt]] ::
          shapeless.HNil,
        laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[laserdisc.OK] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          laserdisc.Maybe[Option[laserdisc.PosInt]] ::
          shapeless.HNil
      ]
    ]
  }
}
