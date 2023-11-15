package scas.structure.conversion

import scas.math.conversion.Equiv

trait Structure[T] extends scas.structure.Structure[T] with Equiv[T]
