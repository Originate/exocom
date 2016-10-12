package exorelay

trait ExoRelayLike {
  val exocomPort: Int
  val serviceName: String
  val config: ExoRelayConfig = ExoRelayConfig(exocomPort, serviceName)
}
