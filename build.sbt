
name := "FIXParser"

version := "1.0"

scalaVersion := "2.12.0"

organization := "deromefintech.com"

resolvers ++= Seq("snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
  "staging"       at "https://oss.sonatype.org/content/repositories/staging",
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
  "releases"      at "https://oss.sonatype.org/content/repositories/releases")

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_"
)

tutSettings

val circeVersion = "0.6.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies ++= {
  Seq(
    "org.scala-lang"    % "scala-compiler"      % scalaVersion.value,
    "org.scala-lang"    % "scala-reflect"       % scalaVersion.value,
    "org.typelevel" %% "cats" % "0.8.1",
    "com.lihaoyi" % "fastparse_2.12" % "0.4.2",
    "org.scalatest" % "scalatest_2.12" % "3.0.1"   % Test,
    "ch.qos.logback"    % "logback-classic"     % "1.0.13"
  )
}