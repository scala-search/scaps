# Scaps: Scala API Search

Scaps is a search engine for discovering functionality in Scala libraries. You can use both type signatures and keywords to search for definitions.

## License

Scaps is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Scaps is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with Scaps.  If not, see http://www.gnu.org/licenses/.

## Development

### Benchmark

Changes to the search algorithm can be evaluated with the test collections defined in `evaluation/src/main/resources/`. To benchmark the core library with the standard parametrization, run

> `sbt ’evaluation/run-main scaps.evaluation.Benchmark’`

### Publish and Install the SBT Plug-in

The SBT plug-in has to be published to a maven repository before it can be used in an SBT project. To publish all artifacts including the plug-in to the local Ivy repository, run:

> `sbt publishLocal`

The plug-in can now be used by creating a new SBT project on **the same machine that also hosts the web service** and including the following line in the `project/plugins.sbt` file:

> `addSbtPlugin("org.scala-search" % "scaps-sbt" % "0.1-SNAPSHOT")`

Additionally, the libraries to index have to be included in the project's `build.sbt` file as a library dependency. For example, to index scalaz, add

> `libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.1"`

The Scala Standard Library is a library dependency per default and does not need to be added in order to be indexed.

If the Scaps service is not exposed at the default ports, the correct hostname can bet set by using

> `scapsHost := "localhost:80"`

to set the location of the User API and

> `scapsControlHost := "localhost:9000"`

to set the location of the Control API.

Finally, an index job can be started by running

> `sbt scapsIndex`

An example project, that demonstrates the required project structure is also given in the `demoEnvironment` directory.

### Run the Web Service

The simplest way to start an instance of the Scaps web service is by using SBT:

> `sbt 'webservice/run'`

This will bind the User API and the Control API to the interfaces and ports as configured in `webservice/src/main/resources/application.conf`. By default, the User API (including the Web UI) is bound to all interfaces on the port `8080` and the Control API is bound to the `localhost` interface on the port `8081`.

For executing the Service in a productive deployment, using SBT is not advised. Instead, the service should be packaged in a standalone, executable jar (see http://www.scala-sbt.org/sbt-native-packager/):

> `sbt 'webservice/debian:packageBin'`

An example configuration for a productive deployment is given in `webservice/src/main/resources/application-prod.conf`.