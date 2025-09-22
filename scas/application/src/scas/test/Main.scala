package scas.test

import java.io.File
import scala.io.Source

val manager = new javax.script.ScriptEngineManager

@main def Main(names: String*) = {
  val file = new File(if names.length > 0 then names(0) else "examples")
  val list = if file.isDirectory then file.listFiles.toSeq else Seq(file)
  val n = (list.map { file =>
    println(file.getName)
    try {
      manager.getEngineByName("scala").eval(Source.fromFile(file).mkString)
      1
    } catch {
      case e : Exception => {
        e.printStackTrace
        println(file.getName + " failure")
        0
      }
    }
  }).foldLeft(0) { (l, r) =>
    l + r
  }
  val m = list.length - n
  println("success : " + n + ", failure : " + m)
  if m > 0 then throw new RuntimeException("tests failed")
}
