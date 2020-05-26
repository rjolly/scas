package scas.rendering

import scas.structure.Structure

class MathObject[T : Structure](x: T) extends jscl.editor.rendering.MathObject {
  override def toString = x.toCode(Structure[T].Level.Addition)
  def toMathML = x.toMathML
}
