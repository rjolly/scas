package scas.structure

trait Module[T, R: Ring] extends AbelianGroup[T] {
  extension (x: R) def  *%(y: T): T
  extension (x: T) def %* (y: R): T

  extension (ring: Ring[R]) def pow(n: Int): Module[T, R]
}
