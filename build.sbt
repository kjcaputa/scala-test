lazy val commonSettings = Seq(
  organization := "net.atos.kjc",
  scalaVersion := "2.11.7",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

lazy val fruitshop = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    version := "0.1.0",
    name := "checkout-system"
  )


