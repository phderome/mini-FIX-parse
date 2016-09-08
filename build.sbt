name := "FIXParser"

version := "1.0"

scalaVersion := "2.11.8"

organization := "deromefintech.com"

resolvers ++= Seq("snapshots"     at "https://oss.sonatype.org/content/repositories/snapshots",
  "staging"       at "https://oss.sonatype.org/content/repositories/staging",
  "releases"      at "https://oss.sonatype.org/content/repositories/releases")

//unmanagedResourceDirectories in Test <+= baseDirectory { _ / "src/main/webapp" }

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_"
)

//ideaExcludeFolders += ".idea"
//ideaExcludeFolders += ".idea_modules"

libraryDependencies ++= {
  val scalaCompiler = "2.11.8"
  Seq(
    "org.scala-lang"    % "scala-compiler"      % scalaCompiler,
    "org.scala-lang"    % "scala-reflect"       % scalaCompiler,
    "org.typelevel" %% "cats" % "0.6.1",
    "com.lihaoyi" %% "fastparse" % "0.3.7",
    "com.lihaoyi" %% "ammonite-ops" % "0.7.6",
    "com.typesafe" % "config" % "1.3.0",
    "org.scalatest"     %% "scalatest"      % "2.2.6"            % Test,
    "ch.qos.logback"    % "logback-classic"     % "1.0.13"
  )
}
