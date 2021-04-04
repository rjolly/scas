package scas.rendering

import scas.structure.Structure
import scas.prettyprint.Show.Level

class MathObject[T : Structure](x: T) extends jscl.editor.rendering.MathObject {
  override def toString = x.toCode(Level.Addition)
  def toMathML = x.toMathML
}
