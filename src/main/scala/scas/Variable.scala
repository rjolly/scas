package scas

import Ordering.{Int, Iterable, String}

class Variable(val name: String, val subscript: Array[Int]) extends Ordered[Variable] {
  def compare (that: Variable) = Variable.compare(this, that)
  override def toString = name + subscript.map("(" + _ + ")").mkString
}

object Variable extends Ordering[Variable] {
  def compare(x: Variable, y: Variable) = {
    val c = String.compare(x.name, y.name)
    if (c < 0) -1
    else if (c > 0) 1
    else Iterable[Int].compare(x.subscript, y.subscript)
  }
  implicit def string2variable(s: String): Variable = apply(s)
  implicit def symbol2variable(s: Symbol): Variable = apply(s.name)
  def apply(name: String, subscript: Int*) = new Variable(name, subscript.toArray)
}
