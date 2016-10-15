package exorelay

/**
  * Configuration options to connect to the Exosphere service
  *
  * @param exocomPort
  * @param serviceName
  */
case class Config(exocomPort: Int,
                  serviceName: String)
