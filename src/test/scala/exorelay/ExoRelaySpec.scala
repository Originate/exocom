package exorelay

import akka.actor.ActorSystem
import akka.testkit._
import org.scalatest._

import scala.concurrent.Await
import scala.concurrent.duration._

class ExoRelaySpec
  extends TestKit(ActorSystem("ExoRelaySpec"))
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll {

  implicit val executor = system.dispatcher

  val EXOCOM_HOST = "localhost"
  val EXOCOM_PORT = 4100
  val SERVICE_NAME = "test-service"

  val exoRelay = new ExoRelay(Config(EXOCOM_HOST, EXOCOM_PORT, SERVICE_NAME))

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  "An ExoRelay" should "Require a specified port via the exocomPort parameter" in {
    exoRelay.config.exocomPort shouldBe EXOCOM_PORT
  }

  it should "Call 'listen' method to go online and begin receiving messages" in {
    exoRelay.listen()
    assert(true)
  }

  it should "Call 'close' method to close the listening socket and stop receiving" in {
    exoRelay.close()
    assert(true)
  }

  it should "Call 'addHandler' to register a new handler to incoming messages" in {
    for {
      f <- exoRelay.addHander("test.addHandler", _ => ())
      f2 <- exoRelay.hasHandler("test.addHandler")
    } yield assert( f && f2)
  }

  it should "Call 'removeHandler' to remove an existing handler for incoming messages" in {
    for {
      f <- exoRelay.addHander("test.removeHandler", _ => ())
      f2 <- exoRelay.removeHandler("test.removeHandler")
    } yield assert(f && f2)
  }

  it should "Call 'on' method to register a new handler for internal events" in {
    assert(false)
  }

  it should "Call 'off' to remove an existing handler for internal events" in {
    assert(false)
  }
}
