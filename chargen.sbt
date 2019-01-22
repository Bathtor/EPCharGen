enablePlugins(BuildInfoPlugin)

name := "EP Character Generator"

organization := "com.lkroll.ep"

version := "1.2.0-SNAPSHOT"

scalaVersion := "2.12.7"

val akkaV = "2.5.17"
val epcompendiumV = "3.3.0-SNAPSHOT"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaV
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaV
libraryDependencies += "com.typesafe.akka" %% "akka-http"   % "10.1.5" 
libraryDependencies += "com.lkroll.ep" %% "epcompendium-core" % epcompendiumV
libraryDependencies += "com.lkroll.ep" %% "epcompendium-data" % epcompendiumV
libraryDependencies += "com.lihaoyi" %% "fastparse" % "1.+"
libraryDependencies += "org.rogach" %% "scallop" % "3.1.+"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.+"
libraryDependencies += "com.typesafe" % "config" % "1.3.+"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.+"
libraryDependencies += "com.lihaoyi" %% "upickle" % "0.6.4"
libraryDependencies += "org.jliszka" %% "probability-monad" % "1.0.1"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"


buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "com.lkroll.ep.chargen"

fork in run := true

mainClass in assembly := Some("com.lkroll.ep.chargen.Main")
assemblyMergeStrategy in assembly := { 
    case PathList("com", "google", "common", xs @ _*)	=> MergeStrategy.first
    case PathList("com", "typesafe", "config", xs @ _*)	=> MergeStrategy.first
    //case n if n.startsWith("reference.conf")			=> MergeStrategy.concat
    case "logback.xml"									=> MergeStrategy.filterDistinctLines
    case x => {
      val oldStrategy = (assemblyMergeStrategy in assembly).value
      oldStrategy(x)
  	}
}
