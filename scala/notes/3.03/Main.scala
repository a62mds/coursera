import List._

object Main extends App {
  def nth[T](n: Int, lst: List[T]): T	=
    if (lst.isEmpty) throw new IndexOutOfBoundsException
    else if (n==0) lst.head
    else nth(n-1, lst.tail)

  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
  println(nth(2, list))
  nth(3, list)
  nth(-1, list)
}
