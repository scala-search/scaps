import sbt._
import Keys._

object Dependencies {
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % Commons.targetedScalaVersion

  val scalatest = "org.scalatest" %% "scalatest" % "2.2.1" % "test"

  val scalazVersion = "7.1.1"
  val scalaz = "org.scalaz" %% "scalaz-core" % scalazVersion

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

  val upickleVersion = "0.2.6"
  val upickle = "com.lihaoyi" %% "upickle" % upickleVersion
  val autowireVersion = "0.2.5"
  val autowire = "com.lihaoyi" %% "autowire" % autowireVersion

  val scalatagsVersion = "0.5.1"
  val scalatags = "com.lihaoyi" %% "scalatags" % scalatagsVersion

  val utestVersion = "0.3.1"
  val utest = "com.lihaoyi" %% "utest" % utestVersion

  val dispatch = "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"

  val coreDependencies = Seq(
    scalaCompiler,
    upickle,
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
    upickle)

  val webserviceDependencies = Seq(
    akkaActor,
    akkaSlf4j,
    sprayCan,
    sprayRouting,
    upickle,
    autowire,
    scalatags)

  val sbtPluginDependencies = Seq(
    dispatch,
    autowire,
    upickle)
}
