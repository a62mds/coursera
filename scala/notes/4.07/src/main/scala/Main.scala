package lists

trait Cons[T] {
  def :: : T
  def head: T
  def tail: Cons[T]
  def isEmpty: Boolean
  def isort: Const[T] = xs match {
    case Empty => Empty
    case y :: ys => isort(ys).insert(y)
  }
  def insert(x: T): Cons[T] = xs match {
    case Empty => x :: Nil
    case y :: ys => if (x < y) x :: xs else if (x == y) y :: ys else y :: ys.insert(x)
  }
}
object Empty extends Cons[T] {
  def head: T = throw new NoSuchElementException("Tried to access Empty.head")
  def tail: Cons[T] = throw new Error("Tried to access Empty.tail")
  def isEmpty: Boolean = true
}
class NonEmpty[T] extends Cons[T] {
  def head: T = this.elem
  def tail: Cons[T] = this.remove(this.head)
  def isEmpty: Boolean = false
}

