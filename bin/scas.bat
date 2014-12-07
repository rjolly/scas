@echo off
set SCAS_HOME=%~dp0..
jrunscript -classpath "%SCALA_HOME%\lib\scala-compiler.jar";"%SCALA_HOME%\lib\scala-xml_2.11-1.0.3.jar";"%SCALA_HOME%\lib\scala-parser-combinators_2.11-1.0.2.jar";"%SCAS_HOME%\lib\txt2xhtml.jar";"%SCAS_HOME%\macros\target\scala-2.11.0-M8\macros_2.11.0-M8-2.1.jar";"%SCAS_HOME%\target\scala-2.11.0-M8\scas_2.11.0-M8-2.1.jar" -l scala %*
