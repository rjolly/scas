package scas.application

import java.io.{File, FileReader}

object Test extends App {
  val mml = MathML("mmlscala.xsl")
  val manager = new javax.script.ScriptEngineManager
  val n = (0 /: (args map task)) { (l, r) => l + r }
  val m = args.length - n
  println("success : " + n + ", failure : " + m)
  def task(path: String) = {
    val file = new File(path)
    val name = file.getName
    println(name)
    val engine = manager.getEngineByName("scala")
    val reader = new FileReader(file)
    val code = mml(reader)
    try {
      engine.eval(code)
      1
    } catch {
      case _ : Throwable => {
        println(name + " failure")
        0
      }
    } finally {
      reader.close
    }
  }
}
