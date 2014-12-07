
Software needed:

- scala 2.11 ( http://www.scala-lang.org/ )
- sbt 0.13.1 ( http://github.com/harrah/xsbt/ )
- jarlister ( http://github.com/rjolly/jarlister/ )

  jarlister $SCALA_HOME/lib/scala-library.jar
  jarlister $SCALA_HOME/lib/scala-xml_2.11-1.0.3.jar
  jarlister $SCALA_HOME/lib/scala-parser-combinators_2.11-1.0.2.jar

To build scas:
  sbt -Dscala.home.local=$SCALA_HOME package macros/package
  jarlister target/scala-2.11.0-M8/scas_2.11.0-M8-2.1.jar


To run scas, add the bin directory to your path, give bin/scas execution privilege (unix), then:
  scas [example.txt]

