// Will consider implementing sets as binary trees
// There are two types of possible trees:
//    -> a tree for the emptyset, and
//    -> a tree consisting of an integer and two sub-trees

object Main extends App {	
  // Class IntSet is abstract and cannot be instantiated:
	//new IntSet
  val t1 =  new NonEmpty(3, Empty, Empty)
  println(t1.toString)
  val t2 = t1.incl(4)
  println(t2.toString)
  val t3 = new NonEmpty(1, Empty, Empty)
  val t4 = new NonEmpty(2, Empty, Empty)
  println((t3 union t4).toString)
  val t5 = (t3 union t4) union t2
  println(t5.toString)
}

// Abstract classes can contain members that are missing an implementation.
// In our case below, neither incl nor contains are defined, only declared.
// Consequently, no instances of an abstract class can be created with the
// operator new
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(that: IntSet): IntSet
}

// "Extension" means that NonEmpty and Empty are subclasses of the base class
// IntSet.
// This implies that the types Empty and NonEmpty conform to the type IntSet,
// i.e. an object of type Empty or NonEmpty can be used whenever an object of
// type IntSet is required.

// Implementation of the emptyset as a subclass of superclass IntSet
// Could argue that there is only a single empty set, so a whole class devoted
// to it, multiple distinct objects of which may be instantiated by a user, is
// superflous
/*
class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def toString = "."
}
*/
// Instead, we can express this case better using an object definition
object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(that: IntSet): IntSet = that
  override def toString = "."
}
// This defines a singleton object named Empty. No other Empty instances can be
// or need to be created. Singleton objects are values, so Empty evaluates to
// itself.

// Implementation of nonempty sets
class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left.contains(x)
    else if (x > elem) right.contains(x)
    else true
  }
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left.incl(x), right)
    else if (x > elem) new NonEmpty(elem, left, right.incl(x))
    else this
  }
  def union(that: IntSet): IntSet = {
    (left.union(right)).union(that).incl(elem)
  }
  override def toString = "{" + left.toString + elem + right.toString + "}"
}
