name := "scas"

version := "2.0"

scalaVersion := "2.9.2"

scalacOptions += "-no-specialization"

mainClass in (Compile, run) := Some("scas.application.PolyPower")
