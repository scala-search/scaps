import sbt._
import Keys._

object Dependencies {
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % Commons.targetedScalaVersion
  val scalaPickling = "org.scala-lang" %% "scala-pickling" % "0.9.1"

  val scalaTest = "org.scalatest" %% "scalatest" % "2.2.1" % "test"

  val luceneVersion = "4.10.4"
  val luceneCore = "org.apache.lucene" % "lucene-core" % luceneVersion
  val luceneAnalyzersCommon = "org.apache.lucene" % "lucene-analyzers-common" % luceneVersion

  val coreDependencies: Seq[ModuleID] = Seq(
    scalaCompiler,
    scalaPickling,
    scalaTest,
    luceneCore,
    luceneAnalyzersCommon
  )
}