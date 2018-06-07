


lazy val commonSettings = Seq(
  organization := "demons", 
  version := "0.3-SNAPSHOT", 
  scalacOptions ++= Seq("-feature"),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

 
lazy val common = ( project in file("enki-common") )
  .settings(commonSettings)
  .settings(name := "enki-common")

lazy val crdts = ( project in file("enki-crdts") )
  .settings(commonSettings)
  .settings(name := "enki-crdts")
  .dependsOn(common)


lazy val routing = ( project in file("enki-routing") )
  .settings(commonSettings)
  .settings(name := "enki-routing")
  .dependsOn(common)


lazy val root = (project in file(".") )
  .settings(commonSettings)
  .settings(name := "enki-all")
  .aggregate(crdts, routing)

