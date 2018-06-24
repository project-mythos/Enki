


lazy val commonSettings = Seq(
  organization := "demons", 
  version := "0.3-SNAPSHOT", 
  scalacOptions ++= Seq("-feature"),
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

 
lazy val common = ( project in file("enki-common") )
  .settings(commonSettings)
  .settings(name := "enki-common")

lazy val crdts = ( project in file("enki-crdt") )
  .settings(commonSettings)
  .settings(name := "enki-crdt")
  .dependsOn(common)



lazy val root = (project in file(".") )
  .settings(commonSettings)
  .enablePlugins(ScalaUnidocPlugin)
  .settings(name := "enki-all")
  .aggregate(common, crdts)

