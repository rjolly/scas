package jas

import scas.variable.Variable
import scas.util.{Conversion, unary_~}
import edu.jas.poly.TermOrder

object PowerProduct {
  def apply[U: Conversion[Variable]](variables: U*)(tord: TermOrder) = new conversion.PowerProduct(variables.map(~_): _*)(tord)
}
