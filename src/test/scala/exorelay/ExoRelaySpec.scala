package exorelay

import akka.actor.ActorSystem
import akka.testkit._
import org.scalatest._

class ExoRelaySpec
  extends TestKit(ActorSystem("ExoRelaySpec"))
  with FlatSpecLike
  with Matchers
  with BeforeAndAfterAll {

  override def afterAll = {
    TestKit.shutdownActorSystem(system)
  }

  def withExoRelay(testCode: ExoRelay => Any) = {
    val exoRelay = new ExoRelay(42, "test-service")
    testCode(exoRelay)
  }

  "An ExoRelay" should "require a specified port via the exocomPort parameter" in {
      val port = 42
      val exorelay = new ExoRelay(port, "test-service")

      exorelay.config.exocomPort shouldBe port
  }
}
