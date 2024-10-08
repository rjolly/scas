package scas.polynomial.ufd

import scala.annotation.tailrec
import scas.base.BigInteger
import BigInteger.given

trait MultivariatePolynomialOverField[T[C, M], C, M] extends PolynomialWithSubresGCD[T, C, M] with PolynomialOverField[T[C, M], C, M]
