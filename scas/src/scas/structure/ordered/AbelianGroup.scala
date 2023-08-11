package scas.structure.ordered

trait AbelianGroup[T] extends scas.structure.AbelianGroup[T] with Structure[T] {
  extension (x: T) def signum = if (x < zero) -1 else if (x > zero) 1 else 0
}

object AbelianGroup {
  trait Ops[T: AbelianGroup] extends scas.structure.AbelianGroup.Ops[T] with Structure.Ops[T]
}
