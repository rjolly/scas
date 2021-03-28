package scas.prettyprint

def println[T: Show](x: T) = System.out.println(x.show)
