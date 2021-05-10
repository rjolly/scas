package jas.conversion

import edu.jas.poly.{ExpVector, TermOrder}
import scas.util.{Conversion, unary_~}
import scas.variable.Variable

class PowerProduct(val variables: Variable*)(tord: TermOrder) extends jas.PowerProduct(tord) with scas.power.conversion.PowerProduct[ExpVector]

object PowerProduct {
  def apply[U: Conversion[Variable]](variables: U*)(tord: TermOrder) = new PowerProduct(variables.map(~_): _*)(tord)
}
