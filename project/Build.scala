import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "scas",
    version := "2.1",
    scalaVersion := "2.11.0-M8",
    scalaHome := Some(file(sys.props("scala.home.local"))),
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

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _))
  )

  lazy val core: Project = Project(
    "scas",
    file("."),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)(s => "org.scala-lang.modules" %% "scala-xml" % "1.0.0-RC7")) ++ Seq(
      libraryDependencies <+= (scalaVersion)(s => "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.0-RC5"))
  ) dependsOn(macros)
}
