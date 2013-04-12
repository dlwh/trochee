organization := "org.scalanlp"

name := "trochee"

resolvers += ScalaToolsSnapshots

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.1"




libraryDependencies += "EPFL" % "lms_2.10.0" % "0.3-SNAPSHOT"

libraryDependencies += "org.spire-math" %% "spire" % "0.3.0"


libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"

scalacOptions += "-Yvirtualize"

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.5" % "test",
  "org.scalanlp" %% "breeze-core" % "0.3-SNAPSHOT",
  "com.nativelibs4java" % "javacl" % "1.0.0-RC3"
)
