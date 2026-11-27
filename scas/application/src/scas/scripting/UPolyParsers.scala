package scas.scripting

import Parsers.*
import scala.annotation.nowarn
import scala.compiletime.deferred
import scas.polynomial.ufd.growable.PolynomialOverUFD
import scas.base.Rational

class UPolyParsers(using PolynomialOverUFD[UPoly, Rational, Array[Int]]) extends UFDParsers[UPoly] {
  override given structure: PolynomialOverUFD[UPoly, Rational, Array[Int]] = summon
  def variables = structure.pp.variables
  var index = -1
  def generator: Parser[UPoly] = Var.parser ^^ {
    case variable if index > -1 && variables(index) == variable => structure.generator(index)
    case variable => {
      structure.extend(variable)
      index = variables.length - 1
      structure.generator(index)
    }
  }
  @nowarn("msg=match may not be exhaustive")
  def solve: Parser[(UPoly, UPoly, Int)] = literal("solve") ~> ("(" ~> expr) ~ ("," ~> generator) ~ opt("," ~> Int.expr) <~ ")" ^^ {
    case equ ~ x ~ option => (equ, x, option match {
      case Some(n) => n.intValue()
      case None => 0
    })
  }
  def base: Parser[UPoly] = Int.base ^^ { x => structure(Rational(x)) } | generator | "(" ~> expr <~ ")"
}
