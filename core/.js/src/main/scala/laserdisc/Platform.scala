package laserdisc

private[laserdisc] object Platform {
  abstract class LaserDiscRuntimeError(message: String)              extends RuntimeException(message, null)
  abstract class LaserDiscRespProtocolDecodingError(message: String) extends RuntimeException(message, null)
  abstract class LaserDiscRespFrameError(message: String)            extends RuntimeException(message, null)
}
