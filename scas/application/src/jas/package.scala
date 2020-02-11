import scas.structure.Ring

package object jas with
  type BigInteger = edu.jas.arith.BigInteger
  given Eql[BigInteger, BigInteger] = Eql.derived

  given BigInteger as Ring[BigInteger] with
    def (x: BigInteger) + (y: BigInteger) = x.sum(y)
    def (x: BigInteger) - (y: BigInteger) = x.subtract(y)
    def (x: BigInteger) * (y: BigInteger) = x.multiply(y)
    def (x: BigInteger) isZero = x == zero
    def (x: BigInteger) isOne = x == one
    def zero = BigInteger(0)
    def one = BigInteger(1)
