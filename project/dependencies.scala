import sbt._
import Keys._

object Dependencies {
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % Commons.targetedScalaVersion
  val scalaPickling = "org.scala-lang" %% "scala-pickling" % "0.9.1"

  val scalatest = "org.scalatest" %% "scalatest" % "2.2.1" % "test"

  val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.1"

  val config = "com.typesafe" % "config" % "1.2.0"

  val rng = "com.nicta" %% "rng" % "1.3.0"

  val luceneVersion = "4.10.4"
  val luceneCore = "org.apache.lucene" % "lucene-core" % luceneVersion
  val luceneAnalyzersCommon = "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion

  val coreDependencies: Seq[ModuleID] = Seq(
    scalaCompiler,
    scalaPickling,
    scalatest,
    scalaz,
    config,
    luceneCore,
    luceneAnalyzersCommon
  )

  val evaluationDependencies: Seq[ModuleID] = Seq(
    scalatest,
    rng)
}