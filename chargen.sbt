enablePlugins(BuildInfoPlugin)

name := "EP Character Generator"

organization := "com.lkroll"

version := "1.4.0"

scalaVersion := "2.13.16"

scalacOptions += "-Xfatal-warnings"
scalacOptions += "-feature"
scalacOptions += "-deprecation"

// resolvers += Resolver.bintrayRepo("lkrollcom", "maven")

//val akkaV = "2.5.17"
val pekkoV = "1.1.4"
val epcompendiumV = "6.2.0"

// libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaV
// libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaV
// libraryDependencies += "com.typesafe.akka" %% "akka-http"   % "10.1.5"
libraryDependencies += "org.apache.pekko" %% "pekko-actor" % pekkoV
libraryDependencies += "org.apache.pekko" %% "pekko-stream" % pekkoV
libraryDependencies += "org.apache.pekko" %% "pekko-http" % "1.2.0"
libraryDependencies += "com.typesafe" % "config" % "1.4.3"
libraryDependencies += "com.lkroll" %% "epcompendium-core" % epcompendiumV
libraryDependencies += "com.lkroll" %% "epcompendium-data" % epcompendiumV
libraryDependencies += "com.lihaoyi" %% "fastparse" % "3.1.1"
libraryDependencies += "org.rogach" %% "scallop" % "5.2.0"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.18"
libraryDependencies += "com.lihaoyi" %% "upickle" % "4.2.1"
libraryDependencies += "org.jliszka" %% "probability-monad" % "1.0.4"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "com.lkroll.ep.chargen"

run / fork := true

assembly / mainClass := Some("com.lkroll.ep.chargen.Main")
assembly / assemblyMergeStrategy := {
  case PathList("com", "google", "common", xs @ _*)   => MergeStrategy.first
  case PathList("com", "typesafe", "config", xs @ _*) => MergeStrategy.first
  //case n if n.startsWith("reference.conf")			=> MergeStrategy.concat
  case "logback.xml" => MergeStrategy.filterDistinctLines
  case x => {
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
  }
}
