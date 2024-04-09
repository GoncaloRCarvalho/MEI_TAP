package tap

trait Tree[+A]

case object EmptyTree extends Tree[Nothing]

final case class Node[A](elem: A, left: Tree[A], right: Tree[A]) extends Tree[A]



object TreeOps:
  def contains[A <: Comparable[A]](t: Tree[A], v: A): Boolean = ???
  
  def insert[A <: Comparable[A]](t: Tree[A], v: A): Tree[A] = ???
