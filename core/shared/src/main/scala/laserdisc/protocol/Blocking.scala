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

import scala.concurrent.duration.*
import scala.util.Try

// how long blocking commands may wait on the server before replying, read from what's sent, so that commands typed at the CLI count too.
// A wait of 0, for as long as it takes, counts as nothing, and so does a transaction, inside which nothing blocks
private[laserdisc] object Blocking {

  // the longest wait BlockingSeconds allows: a longer one is as good as forever, and the sum stays within what a FiniteDuration holds
  private final val longest = 4294967.seconds

  def waitOf(protocols: Iterable[Protocol[?]]): FiniteDuration =
    protocols.foldLeft(Duration.Zero: FiniteDuration)((sum, protocol) => (sum + waitOf(protocol)) min longest)

  def waitOf(protocol: Protocol[?]): FiniteDuration =
    protocol.frames match {
      case Seq(Arr(Bulk(command) :: arguments)) =>
        val words = arguments.collect { case Bulk(word) => word }
        waitAt(command, words).fold(Duration.Zero: FiniteDuration) {
          case (index, true)  => seconds(words.lift(index))
          case (index, false) => milliseconds(words.lift(index))
        }
      case _ => Duration.Zero
    }

  // the same command, waiting for as long as it takes: its wait, which only stands in for that, is set to 0
  def forever[A](timed: Protocol.Single[A]): Protocol.Single[A] = {
    val words = timed.parameters.collect { case Bulk(word) => word }
    waitAt(timed.command, words) match {
      case Some((index, _)) => timed.withParameter(index, "0")
      case None             => throw new IllegalArgumentException(s"${timed.command} doesn't wait")
    }
  }

  // where a command's wait is among its parameters, and whether it's in seconds rather than milliseconds
  private def waitAt(command: String, words: Seq[String]): Option[(Int, Boolean)] =
    command.toUpperCase match {
      case "BLPOP" | "BRPOP" | "BRPOPLPUSH" | "BLMOVE" | "BZPOPMIN" | "BZPOPMAX" if words.nonEmpty => Some(words.size - 1 -> true)
      case "BLMPOP" | "BZMPOP" if words.nonEmpty                                                   => Some(0 -> true)
      // after the source, destination and both sides, before the amount and order
      case "BLMOVEM" if words.size > 4          => Some(4 -> true)
      case "WAIT" | "WAITAOF" if words.nonEmpty => Some(words.size - 1 -> false)
      // the options come before STREAMS, after XREADGROUP's group and consumer, whose names could be BLOCK
      case read @ ("XREAD" | "XREADGROUP") =>
        val skipped = if (read == "XREADGROUP") 3 else 0
        val options = words.drop(skipped).takeWhile(!_.equalsIgnoreCase("STREAMS"))
        val block   = options.indexWhere(_.equalsIgnoreCase("BLOCK"))
        if (block >= 0 && block + 1 < options.size) Some(skipped + block + 1 -> false) else None
      case _ => None
    }

  private def seconds(word: Option[String]): FiniteDuration =
    word.flatMap(w => Try(w.toDouble).toOption).filter(_ > 0d).fold(Duration.Zero: FiniteDuration) { s =>
      if (s >= longest.toSeconds.toDouble) longest else math.ceil(s * 1000d).toLong.millis
    }

  private def milliseconds(word: Option[String]): FiniteDuration =
    word.flatMap(w => Try(w.toLong).toOption).filter(_ > 0L).fold(Duration.Zero: FiniteDuration) { ms =>
      if (ms >= longest.toMillis) longest else ms.millis
    }
}
