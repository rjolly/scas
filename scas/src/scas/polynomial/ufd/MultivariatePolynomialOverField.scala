package scas.polynomial.ufd

import scala.annotation.tailrec
import scas.base.BigInteger
import BigInteger.given

trait MultivariatePolynomialOverField[T[C, M], C, N] extends PolynomialWithSubresGCD[T, C, N] with PolynomialOverField[T[C, Array[N]], C, Array[N]]
