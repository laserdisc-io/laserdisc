package laserdisc
package fs2

final case class ServerTerminatedConnection(redisAddress: RedisAddress)
    extends Platform.LaserDiscRuntimeError(s"Server $redisAddress terminated client connection")
object ServerUnavailable        extends Platform.LaserDiscRuntimeError("No server available")
object ClientTerminated         extends Platform.LaserDiscRuntimeError("Client terminated connection")
object ClientNotStartedProperly extends Platform.LaserDiscRuntimeError("Client trying to publish to a connection not yet established")
final case class NoInFlightRequest(resp: RESP) extends Platform.LaserDiscRuntimeError(s"Got unsolicited message from server: $resp")
final case class InvalidSocketAddress(host: Host, port: Port)
    extends Platform.LaserDiscRuntimeError(s"SocketAddress creation failed. Invalid Host or Port: ${host.value}, ${port.value}")
final case class RequestTimedOut[A](protocol: Protocol.Aux[A]) extends Platform.LaserDiscRuntimeError(s"The request $protocol timed-out")
