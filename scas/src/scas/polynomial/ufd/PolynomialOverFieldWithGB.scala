package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.Field
import scas.module.Array
import scas.base.BigInteger
import BigInteger.given

trait PolynomialOverFieldWithGB[T : ClassTag, C, M] extends PolynomialWithGB[T, C, M] with PolynomialWithModInverse[T, C, M] {
  extension (x: T) def modInverse(mods: T*) = {
    given module: Module[T, C, M] = new Module(using this)("c", 2)
    val s = Seq(Array(x, 1)) ++ mods.map(Array(_, 0))
    val list = module.gb(s*)
    val Array(p, q) = list.head
    assert (p.isUnit)
    q / p
  }
}
