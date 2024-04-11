ThisBuild / version := "0.7"

ThisBuild / scalaVersion := "2.13.13"

organization := "com.iarkdata"
name := "ArkFlow"
publishMavenStyle := true
publishLocal := true
publishTo := Some( Resolver.file("publish-m2-local", Path.userHome / ".m2" / "repository"))

lazy val root = (project in file("."))
  .settings(
    name := "ArkFlow",
  )

////////////////////////////////////////////////////////////////////////////////
// External modules ::
//libraryDependencies += "org.apache.kafka" % "kafka-clients" % "3.6.0"
//libraryDependencies += "org.apache.zookeeper" % "zookeeper" % "3.9.1"
//libraryDependencies += "org.apache.curator" % "curator-framework" % "5.5.0"


// https://mvnrepository.com/artifact/org.json4s/json4s-jackson
libraryDependencies += "org.json4s" %% "json4s-jackson" % "4.1.0-M5"


// parser combinator library
libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.0.2"