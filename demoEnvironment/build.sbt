//enablePlugins(ScalaJSPlugin)

lazy val root = (project in file("."))
	.settings(
		name := "scaps-demo-project",
		version := "1.0",
		scalaVersion := "2.11.7",
		autoAPIMappings := true,
		// scapsControlHost in Scaps := "localhost:9000",
		libraryDependencies += "org.scala-lang" % "scala-library" % "2.11.6",
		libraryDependencies += "org.scala-refactoring" %% "org.scala-refactoring.library" % "0.6.2",
		//libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"
		//libraryDependencies += "org.spire-math" %% "spire" % "0.10.1"
		//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"
		//libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.10"
		//libraryDependencies += "io.spray" %% "spray-can" % "1.3.3"
		//libraryDependencies += "io.spray" %% "spray-client" % "1.3.3"
		//libraryDependencies += "io.spray" %% "spray-http" % "1.3.3"
		//libraryDependencies += "io.spray" %% "spray-httpx" % "1.3.3"
		//libraryDependencies += "io.spray" %% "spray-routing" % "1.3.3"
		(indexDependencies in Scaps) ++= Seq(
			"org.scala-lang" % "scala-library" % scalaVersion.value,
			"org.scala-refactoring" %% "org.scala-refactoring.library" % "0.6.2")
	)