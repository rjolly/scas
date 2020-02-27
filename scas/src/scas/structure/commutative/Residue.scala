package scas.structure.commutative

import scas.prettyprint.Level

abstract class Residue[T: UniqueFactorizationDomain] extends UniqueFactorizationDomain[T] with
  def apply(x: T): T
  def signum(x: T) = UniqueFactorizationDomain[T].signum(x)
  def (x: T) + (y: T) = this(UniqueFactorizationDomain[T].+(x)(y))
  def (x: T) - (y: T) = this(UniqueFactorizationDomain[T].-(x)(y))
  def (x: T) * (y: T) = this(UniqueFactorizationDomain[T].*(x)(y))
  def equiv(x: T, y: T) = UniqueFactorizationDomain[T].equiv(x, y)
  def (x: T).toCode(level: Level) = UniqueFactorizationDomain[T].toCode(x)(level)
  def (x: T).toMathML = UniqueFactorizationDomain[T].toMathML(x)
  def zero = UniqueFactorizationDomain[T].zero
  def one = UniqueFactorizationDomain[T].one
