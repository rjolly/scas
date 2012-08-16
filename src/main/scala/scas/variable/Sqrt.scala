package scas.variable

import scala.xml.Elem

class Sqrt(x: (String, Elem)) extends Function("sqrt", (Array(x._1), Array(x._2))) {
  override def toMathML = <apply><root/>{parameter._2}</apply>
}
