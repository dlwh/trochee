name := "trochee"

resolvers += ScalaToolsSnapshots

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.1"

libraryDependencies += "EPFL" % "lms_2.10.0" % "0.3-SNAPSHOT"

libraryDependencies += "org.spire-math" %% "spire" % "0.3.0"

scalacOptions += "-Yvirtualize"
