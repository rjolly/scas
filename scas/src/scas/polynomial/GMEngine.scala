package scas.polynomial

class GMEngine[T, C, M](using factory: Polynomial[T, C, M]) extends GBEngine[T, C, M] with GMSetting[T, C, M, Pair]
