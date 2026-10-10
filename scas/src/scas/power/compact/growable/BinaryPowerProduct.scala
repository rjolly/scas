package scas.power.compact.growable

import scas.variable.Variable

abstract class BinaryPowerProduct(shift: Int)(variables: Variable*) extends PowerProduct(shift)(variables*) with scas.power.compact.BinaryPowerProduct
