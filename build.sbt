ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "NewServer"
  )
libraryDependencies ++= Seq(
  "io.undertow" % "undertow-core" % "2.2.14.Final" ,
  "io.undertow" % "undertow-servlet" % "2.2.14.Final",
  "io.undertow" % "undertow-websockets-jsr" % "2.2.14.Final",
)

resolvers +=DefaultMavenRepository