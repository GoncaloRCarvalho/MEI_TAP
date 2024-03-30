package tap

object Curry:

  def multiply(a:Int)(b:Int): Int = a * b

  def multiply_by_two: Int => Int = multiply(2)
  def multiply_by_three: Int => Int = multiply(3)