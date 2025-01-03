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

import java.util.concurrent.TimeUnit.SECONDS

import cats.effect.{Resource, Sync}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService}

object MkResource {
  sealed trait CanShutdown[A] { def shutdown[F[_]](implicit F: Sync[F]): A => F[Unit] }

  final object CanShutdown {
    implicit val canShutdownExecutionContextExecutorService: CanShutdown[ExecutionContextExecutorService] =
      new CanShutdown[ExecutionContextExecutorService] {
        override def shutdown[F[_]](implicit F: Sync[F]): ExecutionContextExecutorService => F[Unit] =
          ec =>
            F.delay {
              ec.shutdown()
              ec.awaitTermination(3, SECONDS)
              ()
            }
      }
  }

  private[laserdisc] final def apply[F[_]: Sync, A](acquire: =>F[A])(implicit A: CanShutdown[A]): Resource[F, A] =
    Resource.make(acquire)(A.shutdown)

  /** Creates an execution context that will wait on shut down.
    */
  @inline final def of[F[_]: Sync](fe: F[ExecutionContextExecutorService]): Resource[F, ExecutionContext] =
    MkResource(fe).widenRight[ExecutionContext]
}
