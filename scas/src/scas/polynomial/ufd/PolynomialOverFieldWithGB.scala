package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.Field
import scas.module.Array
import scas.base.BigInteger
import BigInteger.given

trait PolynomialOverFieldWithGB[T[C, M], C, M](using ClassTag[T[C, M]]) extends PolynomialWithGB[T, C, M] with PolynomialWithModInverse[T[C, M], C, M] {
  extension (x: T[C, M]) def modInverse(mods: T[C, M]*) = {
    val s = Seq(Array(x, 1, 0)) ++ mods.map(Array(_, 0, 0))
    val list = module.gb(s*)
    val Array(p, q, _) = list.head
    assert (p.isUnit)
    q / p
  }
}
