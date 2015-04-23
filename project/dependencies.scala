import sbt._
import Keys._

object Dependencies {
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % Commons.targetedScalaVersion
  val scalaPickling = "org.scala-lang" %% "scala-pickling" % "0.9.1"

  val scalatest = "org.scalatest" %% "scalatest" % "2.2.1" % "test"

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.1"

  val config = "com.typesafe" % "config" % "1.2.0"
  val logging = "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
  val logback = "ch.qos.logback" % "logback-classic" % "1.1.3"

  val rng = "com.nicta" %% "rng" % "1.3.0"

  val luceneVersion = "4.10.4"
  val luceneCore = "org.apache.lucene" % "lucene-core" % luceneVersion
  val luceneAnalyzersCommon = "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion

  val akkaVersion = "2.3.9"
  val akkaActor = "com.typesafe.akka" %% "akka-actor" % akkaVersion
  val akkaSlf4j = "com.typesafe.akka" %% "akka-slf4j" % akkaVersion

  val sprayVersion = "1.3.3"
  val sprayCan = "io.spray" %% "spray-can" % sprayVersion
  val sprayRouting = "io.spray" %% "spray-routing" % sprayVersion

  val uPickle = "com.lihaoyi" %% "upickle" % "0.2.6"
  val autowire = "com.lihaoyi" %% "autowire" % "0.2.5"

  val scalatags = "com.lihaoyi" %% "scalatags" % "0.5.1"

  val coreDependencies = Seq(
    scalaCompiler,
    scalaPickling,
    scalatest,
    scalaz,
    config,
    logging,
    logback,
    luceneCore,
    luceneAnalyzersCommon)

  val evaluationDependencies = Seq(
    scalatest,
    rng)

  val webapiDependencies = Seq(
    scalaz)

  val webserviceDependencies = Seq(
    akkaActor,
    akkaSlf4j,
    sprayCan,
    sprayRouting,
    uPickle,
    autowire,
    scalatags)
}
