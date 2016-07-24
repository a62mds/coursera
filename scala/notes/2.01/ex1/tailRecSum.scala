

object tailRecSum extends App {

  val min: Long = 1
  val max: Long = 1000

  println(sum(x => x)(min,max))


  def sum(f: Long => Long)(a: Long, b: Long): Long = {
    def loop(a: Long, acc: Long): Long = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a,0)
  }
}
