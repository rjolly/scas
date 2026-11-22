package scas.polynomial.ufd

import scala.annotation.tailrec
import scas.base.BigInteger
import BigInteger.given

trait MultivariatePolynomialOverField[T[C], C, N] extends PolynomialWithSubresGCD[T, C, N] with PolynomialOverField[T[C], C, Array[N]]
