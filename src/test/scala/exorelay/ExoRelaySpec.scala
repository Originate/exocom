package exorelay

import akka.actor.ActorSystem
import akka.testkit._
import org.scalatest._

class ExoRelaySpec
  extends TestKit(ActorSystem("ExoRelaySpec"))
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll {

  implicit val executor = system.dispatcher

  val EXOCOM_PORT = 42
  val SERVICE_NAME = "test-service"

  val exoRelay = new ExoRelay(Config(EXOCOM_PORT, SERVICE_NAME))

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  "An ExoRelay" should "require a specified port via the exocomPort parameter" in {

    exoRelay.config.exocomPort shouldBe EXOCOM_PORT
  }

  it should "be able to listen on a specied port for incoming messages" in {
    exoRelay.listen()
    exoRelay.listen(4100)
    assert(true)
  }

  it should "be able to add a new handler" in {
    assert(false)
  }

  it should "be able to remove an existing handler" in {
    assert(false)
  }
}
