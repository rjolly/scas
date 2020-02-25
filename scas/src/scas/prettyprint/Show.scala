package scas.prettyprint

trait Show[T] with
  def (x: T).toCode(level: Level): String
  def (x: T).toCode: String = x.toCode(Level.Addition)
  def mml(x: T) = s"<math>${x.toMathML}</math>"
  def (x: T).toMathML: String
