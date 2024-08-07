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
  def apply[U : Conversion[T], T : Show](s: List[U]): MathObject = new MathObject {
    override def toString = s.map(~_).show
    def toMathML = s.map(~_).toMathML
  }
  def apply[T](x: Structure[T]): MathObject = new MathObject {
    override def toString = x.toString
    def toMathML = x.toMathML
  }
}
