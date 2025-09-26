package scas.polynomial.ufd

import scala.compiletime.deferred
import scala.reflect.ClassTag
import scas.math.Numeric
import scas.structure.commutative.Field
import scas.power.growable.ArrayPowerProduct
import scas.module.Array
import scas.variable.Variable
import scas.base.BigInteger
import BigInteger.given

trait PolynomialOverFieldWithGB[T : ClassTag, C, N : {Numeric, ClassTag}] extends PolynomialWithGB[T, C, N] with PolynomialWithModInverse[T, C, Array[N]] {
  given pp: ArrayPowerProduct[N] = deferred
  def newInstance(variables: Variable*): PolynomialOverFieldWithGB[T, C, N]
  extension (x: T) def modInverse(mods: T*) = {
    given module: Module[T, C, N] = new Module(using this)("c", 2)
    val s = Seq(Array(x, 1)) ++ mods.map(Array(_, 0))
    val list = module.gb(s*)
    val Array(p, q) = list.head
    assert (p.isUnit)
    q / p
  }
  extension (ring: Field[C]) override def apply(s: T*): PolynomialOverFieldWithGB[T, C, N] = {
    same(s*)
    this
  }
}
