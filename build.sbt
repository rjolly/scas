name := "scas"

version := "2.1"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-language:higherKinds", "-language:implicitConversions", "-language:postfixOps", "-no-specialization")

mainClass in (Compile, run) := Some("scas.application.PolyPower")
