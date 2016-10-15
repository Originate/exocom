package exorelay

import akka.actor._
import zeromq.ZeroMQExtension

import scala.concurrent.ExecutionContextExecutor

class ExoRelay(override val config: Config)(implicit system: ActorSystem)
  extends ExoRelayLike {

  val zmq = new ZeroMQExtension(system)

  val messageSender: ActorRef = ???
  val messageHandler: ActorRef = ???
  val zmqListener = system.actorOf(ZmqListener.props(messageHandler, zmq))

  def listen(exocomPort: Int = config.exocomPort) = {
    zmqListener ! ZmqListener.Connect(exocomPort)
  }

  def close() = {
    zmqListener ! ZmqListener.Disconnect
  }

  def send(name: String, payload: Any, handler: => Unit) = ???

  def on(event: String, callback: => Unit) = ???
}
