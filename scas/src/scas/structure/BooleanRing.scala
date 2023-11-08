package scas.structure

trait BooleanRing[T] extends Ring[T] {
  extension (x: T) {
    def and(y: T) = x * y
    def or(y: T) = x ^ y ^ (x && y)
    def xor(y: T) = x + y
    def unary_! = x ^ one
    def implies(y: T) = y || !x
    def && (y: T) = x.and(y)
    def || (y: T) = x.or(y)
    def ^ (y: T) = x.xor(y)
    def >> (y: T) = x.implies(y)
  }
}
