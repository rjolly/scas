package scas.structure.conversion

trait Module[T, R: scas.structure.Ring] extends scas.structure.Module[T, R] with AbelianGroup[T]
