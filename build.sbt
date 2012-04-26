import AssemblyKeys._

name := "tutor-toolbox"

version := "0.2-SNAPSHOT"

scalaVersion := "2.9.2"

scalacOptions ++= Seq(
	"-deprecation",
	"-unchecked"
)

libraryDependencies +=
	"org.scalaz" %% "scalaz-core" % "6.0.4"

mainClass := Some("edu.tum.cs.theo.Main")

assemblySettings

jarName in assembly := "tutor-toolbox.jar"
