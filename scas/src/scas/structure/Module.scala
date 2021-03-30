package scas.structure

trait Module[T, R: Ring] extends AbelianGroup[T] {
  extension (x: R) {
    def multiplyLeft(y: T): T
    def *%(y: T): T = x.multiplyLeft(y)
  }
  extension (x: T) {
    def multiplyRight(y: R): T
    def %* (y: R): T = x.multiplyRight(y)
  }
}
