def sumFactorials(a: Int, b: Int): Int =
  if (a > b) 0 else fact(a) + sumFactorials(a + 1, b)
