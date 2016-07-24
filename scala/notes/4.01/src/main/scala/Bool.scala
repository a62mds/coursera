package ideal.scala

abstract class Bool {	
  def ifThenElse[T](then_st: => T, else_st: => T): T

  def && (x: => Bool): Bool = ifThenElse(x, false)
  def || (x: => Bool): Bool = ifThenElse(true, x)
  def unary_!: Bool         = ifThenElse(false, true)

  def == (x: Bool): Bool    = ifThenElse(x, x.unary_!)
  def != (x: Bool): Bool    = ifThenElse(x.unary_!, x)

  def <= (x: Bool): Bool    = ifThenElse(false,x)
}

object true extends Bool {
  def ifThenElse[T](then_st: => T, else_st: => T) then_st
}

object false extends Bool {
  def ifThenElse[T](then_st: => T, else_st: => T) else_st
}


