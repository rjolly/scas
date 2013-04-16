
Software needed:

- scala 2.11 ( http://www.scala-lang.org/ )
- sbt 0.12.1 ( http://github.com/harrah/xsbt/ )
- jarlister ( http://github.com/rjolly/jarlister/ )

To build the libraries:
  sbt package macros/package


To run scas interactively:
  jrunscript -classpath $SCALA_HOME/lib/scala-library.jar:target/scala-2.11/scas_2.11-2.1.jar -l scas


To run the scala examples:
  jarlister $SCALA_HOME/lib/scala-library.jar
  jarlister target/scala-2.11/scas_2.11-2.1.jar
  scala -classpath lib/txt2xhtml.jar:macros/target/scala-2.11/macros_2.11-2.1.jar target/scala-2.11/scas_2.11-2.1.jar examples/*

