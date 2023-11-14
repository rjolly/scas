package scas.prettyprint

trait Show[T] {
  extension (x: T) {
    def show: String
    def toMathML: String
  }
  extension (s: List[T]) {
    def show: String = s.show("List")
    def show(bare: Boolean): String = s.show("")
    def show(key: String): String = s"${key}(${s.map(_.show).mkString(", ")})"
    def toMathML: String = s.toMathML("list")
    def toMathML(tag: String): String = s"<${tag}>${s.map(_.toMathML).mkString}</${tag}>"
  }
}
