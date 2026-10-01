package scas.power.compact.degree

trait ReversePowerProduct extends PowerProduct {
  extension (x: Array[Int]) {
    override def get(i: Int) = super.get(x)(nbvars - 1 - i)
    override def set(i: Int, c: Int) = super.set(x)(nbvars - 1 - i, c)
  }
}
