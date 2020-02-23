package scas.structure

abstract class Quotient[T: Ring] extends Field[(T, T)] with
  def (x: (T, T)) + (y: (T, T)) = x match
    case (a, b) => y match
      case (c, d) => (a * d + c * b, b * d)
  def (x: (T, T)) - (y: (T, T)) = x match
    case (a, b) => y match
      case (c, d) => (a * d - c * b, b * d)
  def (x: (T, T)) * (y: (T, T)) = x match
    case (a, b) => y match
      case (c, d) => (a * c, b * d)
  def (x: (T, T)) / (y: (T, T)) = x match
    case (a, b) => y match
      case (c, d) => (a * d, b * c)
  def zero = (Ring[T].zero, Ring[T].one)
  def one = (Ring[T].one, Ring[T].one)
