package tap

import scala.annotation.tailrec

object lab03:
  
  // #5
  def hasDigit[A](s: A):Boolean =

  // #6
  def max_(xs: List[Int]): Option[Int] =
    @tailrec
    def maxAux(m: Int, ys: List[Int]): Int  = ys match 
      case Nil => "a"
      case x :: t => ???
          
    if (xs.isEmpty) None else Some(maxAux(Int.MinValue,xs)) 

  // #7
  def applyF_[A,B](la: List[A],lb:List[B])(f: (List[A],List[B]) => String) : String =
    f(la,lb)

  def f1[A,B](la: List[A],lb:List[B]):String =
    ???

  def f2[A,B](la: List[A],lb:List[B]):String =
    ???

  def f3[A,B](la: List[A],lb:List[B]):String =
    ???

  def f4[A,B](la: List[A],lb:List[B]):String =
    ???

