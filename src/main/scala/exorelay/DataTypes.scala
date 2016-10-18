package exorelay

/**
  * Exocom Message sent and received
  * @param id
  * @param messageName
  * @param payload
  * @param responseTo
  */
case class ExoMessage(id: String, messageName: String, payload: String, responseTo: Option[String] = None)