package scas.rendering

class Graph(f: Double => Double) extends (Double => Double) with jscl.editor.rendering.Plot {
  def apply(x: Double) = f(x)
}

object Graph {
  def apply(f: Double => Double) = new Graph(f)
}
