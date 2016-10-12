name := "exorelay-scala"

organization := "com.originate"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.4.10"
  val scalaTestV = "3.0.0"
	Seq(
    "com.mdialog"       %% "scala-zeromq" % "1.2.1-SNAPSHOT",
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV % "test",
    "org.scalatest"     %% "scalatest" % scalaTestV % "test")
}

