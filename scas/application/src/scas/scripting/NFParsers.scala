package scas.scripting

import Parsers._
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

  def reset: Unit = {
    ba.reset
  }
}

object NFParsers extends NFParsers(BooleanAlgebra())
