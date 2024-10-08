package scas.polynomial.gb

import scas.polynomial.Polynomial

class GMEngine[T, C, M](using factory: Polynomial[T, C, M]) extends GBEngine[T, C, M] with GMSetting[T, C, M, Pair]
