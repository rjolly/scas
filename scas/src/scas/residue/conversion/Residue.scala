package scas.residue.conversion

import scas.structure.commutative.conversion.Field

trait Residue[T, C, M] extends scas.residue.Residue[T, C, M] with Field[T]
