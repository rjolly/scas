package scas.test

import java.io.File
import scala.io.Source

val manager = new javax.script.ScriptEngineManager

@main def Main(names: String*) = {
  val file = new File(if (names.length > 0) names(0) else "examples")
  val list = if (file.isDirectory) file.listFiles.toSeq else Seq(file)
  val n = list.map(task).foldLeft(0) { (l, r) =>
    l + r
  }
  val m = list.length - n
  println("success : " + n + ", failure : " + m)
}

def task(file: File) = {
  println(file.getName)
  try {
    manager.getEngineByName("scala").eval(Source.fromFile(file).mkString)
    1
  } catch {
    case e : Throwable => {
      e.printStackTrace
      println(file.getName + " failure")
      0
    }
  }
}
