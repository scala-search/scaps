import sbt._
import Keys._

object Commons {
  val appVersion = "1.0"
  val targetedScalaVersion = "2.11.5"

  val settings: Seq[Def.Setting[_]] = Seq(
  	scalaVersion := targetedScalaVersion,
    version := appVersion,
    resolvers += Opts.resolver.mavenLocalFile,
    scalacOptions ++= Seq("-feature")
  )
}