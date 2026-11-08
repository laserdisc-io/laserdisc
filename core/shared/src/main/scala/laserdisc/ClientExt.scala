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

trait ClientExt[F[_], Env] { this: Client[F, Env] =>
  import scala.concurrent.duration.FiniteDuration

  //format: off
  final def send[A1, A2](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: EmptyTuple, (Maybe[A1], Maybe[A2])]
  ): F[(Maybe[A1], Maybe[A2])] = F.map(send(protocolA1 *: protocolA2 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: EmptyTuple, (Maybe[A1], Maybe[A2])]
  ): F[(Maybe[A1], Maybe[A2])] = F.map(send(protocolA1 *: protocolA2 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    protocolA19: Protocol[A19],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: Protocol[A19] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: protocolA19 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    protocolA19: Protocol[A19]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: Protocol[A19] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: protocolA19 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    protocolA19: Protocol[A19],
    protocolA20: Protocol[A20],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: Protocol[A19] *: Protocol[A20] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: protocolA19 *: protocolA20 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    protocolA19: Protocol[A19],
    protocolA20: Protocol[A20]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: Protocol[A19] *: Protocol[A20] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: protocolA19 *: protocolA20 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    protocolA19: Protocol[A19],
    protocolA20: Protocol[A20],
    protocolA21: Protocol[A21],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: Protocol[A19] *: Protocol[A20] *: Protocol[A21] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: Maybe[A21] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: Maybe[A21] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20], Maybe[A21])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20], Maybe[A21])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: protocolA19 *: protocolA20 *: protocolA21 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    protocolA19: Protocol[A19],
    protocolA20: Protocol[A20],
    protocolA21: Protocol[A21]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: Protocol[A19] *: Protocol[A20] *: Protocol[A21] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: Maybe[A21] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: Maybe[A21] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20], Maybe[A21])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20], Maybe[A21])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: protocolA19 *: protocolA20 *: protocolA21 *: EmptyTuple, defaultTimeout))(ev1(_))

  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    protocolA19: Protocol[A19],
    protocolA20: Protocol[A20],
    protocolA21: Protocol[A21],
    protocolA22: Protocol[A22],
    timeout: FiniteDuration
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: Protocol[A19] *: Protocol[A20] *: Protocol[A21] *: Protocol[A22] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: Maybe[A21] *: Maybe[A22] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: Maybe[A21] *: Maybe[A22] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20], Maybe[A21], Maybe[A22])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20], Maybe[A21], Maybe[A22])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: protocolA19 *: protocolA20 *: protocolA21 *: protocolA22 *: EmptyTuple, timeout))(ev1(_))
  final def send[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](
    protocolA1: Protocol[A1],
    protocolA2: Protocol[A2],
    protocolA3: Protocol[A3],
    protocolA4: Protocol[A4],
    protocolA5: Protocol[A5],
    protocolA6: Protocol[A6],
    protocolA7: Protocol[A7],
    protocolA8: Protocol[A8],
    protocolA9: Protocol[A9],
    protocolA10: Protocol[A10],
    protocolA11: Protocol[A11],
    protocolA12: Protocol[A12],
    protocolA13: Protocol[A13],
    protocolA14: Protocol[A14],
    protocolA15: Protocol[A15],
    protocolA16: Protocol[A16],
    protocolA17: Protocol[A17],
    protocolA18: Protocol[A18],
    protocolA19: Protocol[A19],
    protocolA20: Protocol[A20],
    protocolA21: Protocol[A21],
    protocolA22: Protocol[A22]
  )(
    implicit F: Functor[F],
    ev0: Handler.Aux[F, Env, Protocol[A1] *: Protocol[A2] *: Protocol[A3] *: Protocol[A4] *: Protocol[A5] *: Protocol[A6] *: Protocol[A7] *: Protocol[A8] *: Protocol[A9] *: Protocol[A10] *: Protocol[A11] *: Protocol[A12] *: Protocol[A13] *: Protocol[A14] *: Protocol[A15] *: Protocol[A16] *: Protocol[A17] *: Protocol[A18] *: Protocol[A19] *: Protocol[A20] *: Protocol[A21] *: Protocol[A22] *: EmptyTuple, Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: Maybe[A21] *: Maybe[A22] *: EmptyTuple],
    ev1: Tupled[Maybe[A1] *: Maybe[A2] *: Maybe[A3] *: Maybe[A4] *: Maybe[A5] *: Maybe[A6] *: Maybe[A7] *: Maybe[A8] *: Maybe[A9] *: Maybe[A10] *: Maybe[A11] *: Maybe[A12] *: Maybe[A13] *: Maybe[A14] *: Maybe[A15] *: Maybe[A16] *: Maybe[A17] *: Maybe[A18] *: Maybe[A19] *: Maybe[A20] *: Maybe[A21] *: Maybe[A22] *: EmptyTuple, (Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20], Maybe[A21], Maybe[A22])]
  ): F[(Maybe[A1], Maybe[A2], Maybe[A3], Maybe[A4], Maybe[A5], Maybe[A6], Maybe[A7], Maybe[A8], Maybe[A9], Maybe[A10], Maybe[A11], Maybe[A12], Maybe[A13], Maybe[A14], Maybe[A15], Maybe[A16], Maybe[A17], Maybe[A18], Maybe[A19], Maybe[A20], Maybe[A21], Maybe[A22])] = F.map(send(protocolA1 *: protocolA2 *: protocolA3 *: protocolA4 *: protocolA5 *: protocolA6 *: protocolA7 *: protocolA8 *: protocolA9 *: protocolA10 *: protocolA11 *: protocolA12 *: protocolA13 *: protocolA14 *: protocolA15 *: protocolA16 *: protocolA17 *: protocolA18 *: protocolA19 *: protocolA20 *: protocolA21 *: protocolA22 *: EmptyTuple, defaultTimeout))(ev1(_))
  //format: on
}
