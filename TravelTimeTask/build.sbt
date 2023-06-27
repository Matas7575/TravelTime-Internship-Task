ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.11"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.6.11"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test

lazy val root = (project in file("."))
  .settings(
    name := "HelloWorld"
  )
