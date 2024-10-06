package scas.polynomial

class GMEngine[T, C, N](using factory: PolynomialWithGB[T, C, N]) extends GBEngine[T, C, N] with GMSetting[T, C, N, Pair]
