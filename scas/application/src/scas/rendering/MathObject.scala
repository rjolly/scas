package scas.rendering

import jscl.editor.rendering.MathObject
import scas.util.{Conversion, unary_~}
import scas.prettyprint.Show

object MathObject {
  def apply[U : Conversion[T], T : Show](x: U): MathObject = new MathObject {
    override def toString = (~x).show
    def toMathML = (~x).toMathML
  }
  def apply[T : Show](s: List[T]): MathObject = new MathObject {
    override def toString = s.show
    def toMathML = s.toMathML
  }
}
