package scas.scripting

import Parsers._
import scala.annotation.nowarn
import scas.residue.BooleanAlgebra
import scas.base.BigInteger
import Factors.Element

type NF = Element[BA, Int]

class NFParsers(using var structure: NormalForm) extends RingParsers[NF] {
  def this(ring: BooleanAlgebra) = this(using new NormalForm(using ring))
  val ba = new BAParsers(using structure.ring)

  def base: Parser[NF] = ("factor") ~ ("(" ~> ba.expr) <~ ")" ^^ {
    case "factor" ~ x if (ba.structure == structure.ring) => structure.factor(x)
    case "factor" ~ x => {
      structure = new NormalForm(using ba.structure)
      structure.factor(x)
    }
  }
  override def unsignedFactor: Parser[NF] = base ~ opt(("**") ~> Int.unsignedFactor) ^^ {
    case x ~ option => option match {
      case Some(exp) => x.convert \ exp
      case None => x
    }
  }
  override def factor: Parser[NF] = opt("-") ~ unsignedFactor ^^ {
    case option ~ factor => option match {
      case Some(sign) => -factor.convert
      case None => factor
    }
  }
  override def unsignedTerm: Parser[NF] = unsignedFactor ~ rep("*" ~ factor) ^^ {
    case factor ~ list => list.foldLeft(factor) {
      case (x, "*" ~ y) => x.convert * y.convert
    }
  }
  override def term: Parser[NF] = opt("-") ~ unsignedTerm ^^ {
    case option ~ term => option match {
      case Some(sign) => -term.convert
      case None => term
    }
  }
  override def expr: Parser[NF] = term ~ rep(("+" | "-") ~ unsignedTerm) ^^ {
    case term ~ list => list.foldLeft(term) {
      case (x, "+" ~ y) => x.convert + y.convert
      case (x, "-" ~ y) => x.convert - y.convert
    }
  }
  @nowarn("msg=match may not be exhaustive")
  override def comparison: Parser[Boolean] = expr ~ ("=" | "<>") ~ expr ^^ {
    case x ~ "=" ~ y => x.convert >< y.convert
    case x ~ "<>" ~ y => x.convert <> y.convert
  }

  def reset: Unit = {
    ba.reset
  }
}

object NFParsers extends NFParsers(BooleanAlgebra())
