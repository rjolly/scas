package scas.prettyprint

trait Show[T] with
  type Level = Show.Level
  val Level = Show.Level
  def (x: T).toCode(level: Level): String
  def (x: T).toCode: String = x.toCode(Level.Addition)
  def mml(x: T) = s"<math>${x.toMathML}</math>"
  def (x: T).toMathML: String

object Show
  enum Level with
    case Addition, Multiplication, Power
