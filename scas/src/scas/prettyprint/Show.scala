package scas.prettyprint

trait Show[T] {
  def fenced(s: String) = s"($s)"
  export scas.prettyprint.Level
  extension (x: T) {
    def toCode(level: Level): String
    def show = x.toCode(Level.Addition)
    def math = tagged(x.toMathML)
    def toMathML: String
  }
  extension (s: List[T]) {
    def show: String = s"List(${s.map(_.show).mkString(", ")})"
    def math = tagged(s"<list>${s.map(_.toMathML).mkString}</list>")
  }
  def tagged(s: String) = s"<math>$s</math>"
}
