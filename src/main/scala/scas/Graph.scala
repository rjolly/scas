package scas

class Graph(f: Double => Double) extends (Double => Double) with Serializable {
  def apply(x: Double) = f(x)
}

object Graph {
  def apply(f: Double => Double) = new Graph(f)
}
