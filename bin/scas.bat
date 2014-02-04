@echo off
set SCAS_HOME=%~dp0..
set JAVA_OPTS=-Dscas.home="%SCAS_HOME%"
scala -nc -classpath "%SCAS_HOME%\lib\txt2xhtml.jar";"%SCAS_HOME%\macros\target\scala-2.11.0-M8\macros_2.11.0-M8-2.1.jar";"%SCAS_HOME%\target\scala-2.11.0-M8\scas_2.11.0-M8-2.1.jar" %*
