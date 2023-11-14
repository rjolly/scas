package scas.prettyprint

def println[T : Show](x: T) = System.out.println(x.show)
def println[T : Show](s: List[T]) = System.out.println(s.show)
