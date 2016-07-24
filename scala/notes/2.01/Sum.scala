import scala.annotation.tailrec

object Sum extends App {

  val lowerBound: Int = 1
  val upperBound: Int = 5
  val list = List.range(lowerBound, upperBound)
/*
  println(sum(list))
  println(sum2(list))
  println(sum3(list))
  println(sumWithReduce(list))
*/
  printInt(sum(list))

  printSum(4, 5)

  def printInt(anInt: Int): Unit = {
    println(anInt)
  }

  def printSum(lhs: Int, rhs: Int): Unit = {
    println(s"$lhs + $rhs")
  }

  def printEq(lhs: Int, rhs: Int): Unit = {
    println(s"$lhs = $rhs")
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case x :: tail => x + sum(tail)
  }

  def sum2(ints: List[Int]): Int = {
    @tailrec
    def sumAccumulator(ints: List[Int], accum: Int): Int = {
      ints match {
        case Nil => accum
        case x :: tail => sumAccumulator(tail, accum + x)
      }
    }
    sumAccumulator(ints, 0)
  }

  def sum3(xs: List[Int]): Int = {
    if (xs.isEmpty) 0
    else xs.head + sum3(xs.tail)
  }

  def sumWithReduce(ints: List[Int]) = {
    ints.reduceLeft(_ + _)
  }
}
