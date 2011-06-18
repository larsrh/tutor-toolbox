name := "Tutor-Toolbox"

version := "0.1"

scalaVersion := "2.9.0-1"

libraryDependencies +=
	"org.scalaz" %% "scalaz-core" % "6.0.1"

seq(ProguardPlugin.proguardSettings :_*)

proguardOptions += keepMain("theo.re.Main")
