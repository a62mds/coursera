def sum(f: Int => Int, a: Int, b: Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a + 1, b)

  /*
// using auxiliary functions:
def sumInts(a: Int, b: Int)       = sum(id, a, b)
def sumSquares(a: Int, b: Int)    = sum(squares, a, b)
def sumCubes(a: Int, b: Int)      = sum(cube, a, b)
def sumFactorials(a: Int, b: Int) = sum(fact, a, b)

def id(x: Int): Int     = x
def square(x: Int): Int = x * x
def cube(x: Int): Int   = x * x * x
def fact(x: Int): Int   = if (x == 0) 1 else x * fact(x - 1)
*/

// using anonymous functions:
def sumInts(a: Int, b: Int)  = sum(x => x, a, b)
def sumCubes(a: Int, b: Int) = sum(x => x*x*x, a, b)
