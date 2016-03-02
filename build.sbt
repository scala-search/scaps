// Common Settings

scalaVersion := Commons.targetedScalaVersion

lazy val utestFramework = new TestFramework("utest.runner.Framework")

lazy val commonSettings = Seq(
    organization := "org.scala-search",
    scalaVersion := Commons.targetedScalaVersion,
    resolvers += Opts.resolver.mavenLocalFile,
    testFrameworks += utestFramework,
    scalacOptions ++= Seq(
      "-encoding", "UTF8",
      "-Xlint",
      "-feature",
      "-deprecation",
      "-Ywarn-value-discard",
      "-Xfatal-warnings"),
    testModules := Seq(),
    resourceGenerators in Test <+= createTestModules)

// Release

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    runTest,
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    setNextVersion,
    commitNextVersion,
    pushChanges)

// Root Project

lazy val root = (project in file("."))
  .aggregate(nucleus, buildInfo_2_10, buildInfo_2_11, buildInfo_JS, api_2_11, apiJS, core, evaluation, webservice, webserviceUI, scalaClient, sbtPlug)
  .settings(commonSettings: _*)
  .settings(
    publishArtifact := false)

// Sub Projects

// Build Infos

def buildInfoSettings =
  commonSettings ++ Seq(
    name := "scaps-build",
    target := baseDirectory.value / s"target-${scalaVersion.value}",
    buildInfoKeys := Seq[BuildInfoKey](
      version, 
      organization),
    buildInfoPackage := "scaps.buildInfo")

lazy val buildInfo_2_10 = (project in file("buildInfo"))
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoSettings: _*)
  .settings(
    scalaVersion := Commons.sbtPluginScalaVersion,
    test in Test := {})

lazy val buildInfo_2_11_cross = (crossProject in file("buildInfo"))
  .enablePlugins(BuildInfoPlugin)
  .settings(buildInfoSettings: _*)
  .settings(
    scalaVersion := Commons.targetedScalaVersion,
    test in Test := {})
  .jsSettings(
    name := "scaps-build-js")

lazy val buildInfo_2_11 = buildInfo_2_11_cross.jvm
lazy val buildInfo_JS = buildInfo_2_11_cross.js

// Nucleus

lazy val nucleus = (project in file("nucleus"))
  .settings(commonSettings: _*)
  .settings(
    name := "scaps-nucleus",
    libraryDependencies ++= Dependencies.nucleusDependencies)

// API

lazy val api_2_11_cross = (crossProject in file("api"))
  .settings(commonSettings: _*)
  .settings(
    scalaVersion := Commons.targetedScalaVersion,
    name := "scaps-api",
    libraryDependencies ++= Dependencies.apiDependencies ++ 
      Seq("com.lihaoyi" %%% "utest" % Dependencies.utestVersion % "test"),
    target := baseDirectory.value / s"target-${scalaVersion.value}")
  .jsSettings(
    name := "scaps-api-js")

lazy val api_2_11 = api_2_11_cross.jvm
lazy val apiJS = api_2_11_cross.js

// Scala Client

lazy val scalaClient = (project in file("scala"))
  .dependsOn(api_2_11)
  .settings(commonSettings: _*)
  .settings(
    name := "scaps-scala",
    libraryDependencies ++= Dependencies.scalaClientDependencies,
    testModules += "jarExtractorTests")

// Core

lazy val core = (project in file("core"))
  .dependsOn(api_2_11, scalaClient % "test->compile;test->test")
  .settings(commonSettings: _*)
  .settings(
    name := "scaps-core",
    libraryDependencies ++= Dependencies.coreDependencies)

// Evaluation

lazy val evaluation = (project in file("evaluation"))
  .dependsOn(core, scalaClient)
  .settings(commonSettings: _*)
  .settings(
    name := "scaps-evaluation",
    publishArtifact := false,
    libraryDependencies ++= Dependencies.evaluationDependencies)

// Webservice

lazy val webserviceShared_cross = (crossProject in file("webserviceShared"))
  .dependsOn(buildInfo_2_11_cross, api_2_11_cross)
  .settings(commonSettings: _*)
  .settings(
    name := "scaps-webservice-shared",
    publishArtifact := true,
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "scalatags" % Dependencies.scalatagsVersion))
  .jsSettings(
    name := "scaps-webservice-shared-js")

lazy val webserviceShared_JVM = webserviceShared_cross.jvm
lazy val webserviceShared_JS = webserviceShared_cross.js

lazy val webservice = (project in file("webservice"))
  .dependsOn(webserviceShared_JVM, core)
  .enablePlugins(JavaServerAppPackaging, DebianPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "scaps-webservice",
    libraryDependencies ++= Dependencies.webserviceDependencies,
    // parallel execution does not play well with the actor tests
    parallelExecution in Test := false,
    (resources in Compile) ++= Seq(
      (fastOptJS in (webserviceUI, Compile)).value.data,
      (fullOptJS in (webserviceUI, Compile)).value.data),
    testModules := Seq("testModule1", "testModule2"),

    // packager
    javaOptions in Universal ++= Seq(
      "-J-Xmx4g"),

    packageDescription in Debian := "Scaps Webservice",
    maintainer in Debian := "Lukas Wegmann",

    mappings in Universal ++= {
      val conf = (resourceDirectory in Compile).value / "application-prod.conf"
      val log = (resourceDirectory in Compile).value / "logback.xml"
      Seq(
        conf -> "conf/application.conf",
        log -> "conf/logback.xml")
    },
    bashScriptExtraDefines += """addJava "-Dconfig.file=${app_home}/../conf/application.conf" """,
    bashScriptExtraDefines += """addJava "-Dlogback.configurationFile=${app_home}/../conf/logback.xml" """)

lazy val webserviceUI = (project in file("webserviceUI"))
  .dependsOn(webserviceShared_JS)
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings: _*)
  .settings(
    name := "scaps-webservice-ui",
    publishArtifact := false,
    // disable fatal warnings in this project because there are some unavoidable warnings
    scalacOptions := scalacOptions.value.filter(_ != "-Xfatal-warnings"),
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "upickle" % Dependencies.upickleVersion,
      "com.lihaoyi" %%% "autowire" % Dependencies.autowireVersion,
      "com.lihaoyi" %%% "scalatags" % Dependencies.scalatagsVersion,
      "org.scala-js" %%% "scalajs-dom" % "0.8.0",
      "com.lihaoyi" %%% "utest" % Dependencies.utestVersion % "test",
      "org.monifu" %%% "monifu" % "1.0-M1"))

// SBT Plugin

lazy val sbtPlug = (project in file("sbtPlugin"))
  .dependsOn(buildInfo_2_10)
  .settings(commonSettings: _*)
  .settings(
    name := "scaps-sbt",
    scalaVersion := Commons.sbtPluginScalaVersion,
    sbtPlugin := true,
    libraryDependencies ++= Dependencies.sbtPluginDependencies)

// Custom tasks and settings

lazy val testModules = settingKey[Seq[String]]("Folder names of test modules")

lazy val createTestModules = Def.task {
  for(packageName <- (testModules in Test).value) yield {
    val jar = (resourceManaged in Test).value / (packageName + ".jar")
    val packageDir = (resourceDirectory in Test).value / packageName

    streams.value.log.info(s"compress $packageDir into $jar")

    IO.delete(jar)
    def entries(f: File): List[File] =
      f :: (if (f.isDirectory) IO.listFiles(f).toList.flatMap(entries(_)) else Nil)

    val files = entries(packageDir).map(d => (d, d.getAbsolutePath.substring(packageDir.getParent.length +1)))

    IO.zip(files, jar)

    jar
  }
}