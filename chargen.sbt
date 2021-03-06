enablePlugins(BuildInfoPlugin)

name := "EP Character Generator"

organization := "com.lkroll.ep"

version := "1.3.3"

scalaVersion := "2.12.10"

scalacOptions += "-Xfatal-warnings"
scalacOptions += "-feature"

resolvers += Resolver.bintrayRepo("lkrollcom", "maven")

val akkaV = "2.5.17"
val epcompendiumV = "6.0.0"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaV
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaV
libraryDependencies += "com.typesafe.akka" %% "akka-http"   % "10.1.5" 
libraryDependencies += "com.lkroll.ep" %% "epcompendium-core" % epcompendiumV
libraryDependencies += "com.lkroll.ep" %% "epcompendium-data" % epcompendiumV
libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.1.+"
libraryDependencies += "org.rogach" %% "scallop" % "3.2.+"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.+"
libraryDependencies += "com.typesafe" % "config" % "1.3.+"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.+"
libraryDependencies += "com.lihaoyi" %% "upickle" % "0.7.+"
libraryDependencies += "org.jliszka" %% "probability-monad" % "1.0.+"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"


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
