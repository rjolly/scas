package scas.polynomial.ufd

import scala.reflect.ClassTag
import scas.structure.commutative.Field
import scas.module.Array
import scas.base.BigInteger
import BigInteger.given

trait PolynomialOverFieldWithGB[T[M], C, M](using ClassTag[T[M]]) extends PolynomialWithGB[T, C, M] with PolynomialWithModInverse[T[M], C, M] {
  extension (x: T[M]) def modInverse(mods: T[M]*) = {
    val s = Seq(Array(x, 1, 0)) ++ mods.map(Array(_, 0, 0))
    val list = module.gb(s*)
    val Array(p, q, _) = list.filter { r =>
      val Array(p, _, _) = r
      !p.isZero
    }.head
    assert (p.isUnit)
    q / p
  }
}
