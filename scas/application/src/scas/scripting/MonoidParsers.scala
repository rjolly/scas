package scas.scripting

import Parsers._

trait MonoidParsers[T] extends StructureParsers[T] {
  given structure: scas.structure.Monoid[T]
  def base: Parser[T]
  def factor: Parser[T] = base ~ opt(("**" | "^") ~> Int.unsignedFactor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x \ exp
      case None => x
    }
  }
  def expr: Parser[T] = factor ~ rep("*" ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x * y
    }
  }
}
