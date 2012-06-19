package scas

import scala.xml.Elem

trait MathObject {
  def toMathML: Elem
}
