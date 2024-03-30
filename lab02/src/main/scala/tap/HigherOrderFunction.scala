package tap
object HigherOrderFunction:
  def isEven(x: Int) = x % 2 == 0
  def isOdd(x: Int) = x % 2 != 0
  def square(x: Int) = x * x
  def cube(x: Int) = x * x * x

  def isPrime(n: Int): Boolean =
    if (n <= 1)
      false // Numbers less than or equal to 1 are not prime
    else if (n == 2)
      true // 2 is a prime number
    else
      // Check divisibility by numbers from 2 to the square root of n
      val sqrtN = math.sqrt(n).toInt
      !(2 to sqrtN).exists(x => n % x == 0)
  def hof( f :Int => Boolean, m : Int => Int, xs :List[Int] ): List[Int] =
    xs.filter(f).map(m)