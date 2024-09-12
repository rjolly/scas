package scas.scripting

import scas.residue.BooleanAlgebra

class NormalForm(using val ring: BooleanAlgebra) extends Factors[BA, Int] {
  def factor(x: BA) = ring.gb(!x).foldLeft(one)((l, a) => if (!a.isZero) l * this(!a) else l)
  extension (x: NF) def convert = x.map((a, b) => (a.convert, b))
}
