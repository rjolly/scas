package scas.scripting

import Parsers._
import scas.residue.BooleanAlgebra
import scas.base.BigInteger
import Factors.Element

type NF = Element[BA, Int]

class NFParsers(using var structure: NormalForm) extends RingParsers[NF] {
  def this(ring: BooleanAlgebra) = this(using new NormalForm(using ring))
  val ba = new BAParsers(using structure.ring)

  def base: Parser[NF] = ba.expr ^^ {
    case x if (ba.structure == structure.ring) => structure(x)
    case x => {
      structure = new NormalForm(using ba.structure)
      structure(x)
    }
  }
  override def expr: Parser[NF] = base

  def reset: Unit = {
    ba.reset
  }
}

object NFParsers extends NFParsers(BooleanAlgebra())
