package scas.prettyprint

trait Show[T] {
  def fenced(s: String) = s"($s)"
  export scas.prettyprint.Level
  extension (x: T) {
    def toCode(level: Level): String
    def show = x.toCode(Level.Addition)
    def toMathML: String
  }
  extension (s: List[T]) {
    def show: String = s"List(${s.map(_.show).mkString(", ")})"
    def show(bare: Boolean): String = s.show.substring("List".length)
    def toMathML: String = s"<list>${s.map(_.toMathML).mkString}</list>"
  }
  def toMathML: String
}
