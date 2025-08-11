package scas.rendering

import scas.util.{Conversion, unary_~}
import scas.structure.Structure
import scas.prettyprint.Show

type MathObject = jscl.editor.rendering.MathObject

object MathObject {
  def apply[U : Conversion[T], T : Show](x: U): MathObject = new MathObject {
    override def toString = (~x).show
    def toMathML = (~x).toMathML
  }
  def apply[T](x: Structure[T]): MathObject = new MathObject {
    override def toString = x.toString
    def toMathML = x.toMathML
  }
  def apply(f: Double => Double) = Graph(f)
  def apply(s: String) = s
}
