val circe = Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
  ).map(_ % "0.6.1")


val scalaOptions = Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-language:_"
)

lazy val commonSettings = Seq(
  organization := "deromefintech.com",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.3"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "FIXParser",
    libraryDependencies ++= {
      circe ++
      Seq(
        "org.scala-lang" % "scala-compiler"      % scalaVersion.value,
        "org.scala-lang" % "scala-reflect"       % scalaVersion.value,
        "org.typelevel" %% "cats" % "0.8.1",
        "com.lihaoyi"   %% "fastparse" % "0.4.2",
        "org.scalatest" %% "scalatest" % "3.0.1"   % Test,
        "ch.qos.logback"    % "logback-classic"     % "1.0.13"
      )
    },
    resolvers ++= Seq(
      "snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
      "staging"       at "https://oss.sonatype.org/content/repositories/staging",
      "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
      "releases"      at "https://oss.sonatype.org/content/repositories/releases"),
    scalacOptions ++= scalaOptions,
    tutSettings
  )
