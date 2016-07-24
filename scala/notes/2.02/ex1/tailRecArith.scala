
object tailRecArith extends App {
  
  val min: Int = 1
  val max: Int = 4
  println(sum(x => x)(min, max))
  println(prd(x => x)(min, max))
  println(fact(5))

  def sum(f: Int => Int)(min: Int, max: Int): Int = mapCompose(f, (m, n) => m + n, 0)(min, max)

  def prd(f: Int => Int)(min: Int, max: Int): Int = mapCompose(f, (m, n) => m * n, 1)(min, max)
  
  def mapCompose(f: Int => Int, op: (Int, Int) => Int, unit: Int)(min: Int, max: Int): Int = {
    def loop(num: Int, acc: Int): Int = {
      if (num > max) acc
      else loop(num+1, op(f(num),acc))
    }
    loop(min,unit)
  }

  def fact(n: Int): Int = prd(x => x)(1,n)

}
