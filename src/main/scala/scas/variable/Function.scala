package scas.variable

import scala.xml.Elem

class Function(val name: String, val parameter: (Array[String], Array[Elem])) extends Variable {
  override def equals(other: Any) = other match {
    case other: Function => {
      if (!this.name.equals(other.name)) true else (true /: this.parameter._1.zip(other.parameter._1)) { (l, r) =>
        val (a, b) = r
        l && a.equals(b)
      }
    }
  }
  override def hashCode = (name.hashCode /: parameter._1.map(_.hashCode)) {
    (l, r) => l * 31 + r
  }
  override def toString = name + "(" + parameter._1.mkString(", ") + ")"
  def toMathML = <apply><ci>{mml(name)}</ci>{parameter._2}</apply>
}
