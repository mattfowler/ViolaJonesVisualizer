name := "ViolaJonesVisuializer"

version := "1.0"

lazy val `violajonesvisuializer` = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"

libraryDependencies ++= Seq( jdbc , cache , ws   , specs2 % Test )

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.0"


libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

