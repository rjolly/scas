import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "scas",
    version := "2.1",
    scalaVersion := "2.11.0-M3",
    scalacOptions ++= Seq(
      "-language:higherKinds",
      "-language:implicitConversions",
      "-language:postfixOps",
      "-language:experimental.macros"
    )
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "root",
    file("."),
    settings = buildSettings
  ) aggregate(macros, core)

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _))
  )

  lazy val core: Project = Project(
    "scas",
    file("."),
    settings = buildSettings
  ) dependsOn(macros)
}
