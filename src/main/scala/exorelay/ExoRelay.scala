package exorelay

import akka.actor.ActorSystem

class ExoRelay(override val exocomPort: Int,
               override val serviceName: String)(implicit system: ActorSystem)
  extends ExoRelayLike {

}
