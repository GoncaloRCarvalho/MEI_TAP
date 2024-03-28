import scala.annotation.tailrec

//======Conditional Expressions======
def lessThan(a: Int, b: Int) =
  a < b

lessThan(1, 2)
lessThan(3, 2)
lessThan(2, 2)

//==================================

def and(a: Boolean, b: Boolean) =
  if (a) a == b else false

def or(a: Boolean, b: Boolean) =
  if (a) a else if (b) b else false

assert(and(true, true))
assert(!and(true, false))
assert(!and(false, true))
assert(!and(false, false))
assert(!or(false, false))
assert(or(true, true))
assert(or(true, false))
assert(or(false, true))
println("tests passed")

//=================Recursion========================

@tailrec
def sumDown(x: Int, sum: Int): Int =
  if (x > 0) sumDown(x - 1, sum + x) else sum

// Test
assert(sumDown(5, 0) == 15)
assert(sumDown(6, 0) == 21)

// ------------------------------------------------
@tailrec
def nSymbol(i: Int, c: Char, s: String): String =
  if (i > 0) nSymbol(i - 1, c, s + c) else s

// Test
assert(nSymbol(5, '*', "") == "*****")

// ------------------------------------------------
def mult(x: Int, y: Int): Int =
  if (y == 0) 0
  else if (y > 0) x + mult(x, y - 1)
  else -x + mult(x, y + 1)

// Test
assert(mult(4, 3) == 12)
assert(mult(0, 0) == 0)
assert(mult(0, 1) == 0)
assert(mult(1, 0) == 0)
assert(mult(-3, 4) == -12)
assert(mult(3, -4) == -12)
assert(mult(-3, -3) == 9)

// ------------------------------------------------
@tailrec
def GCD(a: Int, b: Int): Int =
  if (a % b == 0) b else GCD(b, a % b)


// Test
assert(GCD(16, 28) == 4)
assert(GCD(206, 40) == 2)

// ------------------------------------------------
def pascal(row: Int, col: Int): Int = {
  if (col == row) 1
  else if (col == 1) 1
  else if (row == 1) 1
  else pascal(row - 1, col - 1) + pascal(row - 1, col)
}

def pascal_row(row: Int, maxCols: Int): String = {
  val srow =
    for {
      col <- 1 to row
      v = pascal(row, col)
    } yield s"$v "
  " " * (maxCols - row) + srow.mkString
}

def pascal_triangle(n: Int): String = {
  val stri =
    for {
      row <- 1 to n
    } yield pascal_row(row, n)
  "\n" + stri.mkString("\n")
}

pascal_triangle(5)

//===============Tail Recursion=====================
def multTail(x: Int, y: Int): Int = {

  @tailrec
  def mult_aux(acc: Int, x: Int, y: Int): Int = {
    if (y == 0) acc
    else if (y > 0) mult_aux(acc + x, x, y - 1)
    else mult_aux(acc - x, x, y + 1)
  }

  mult_aux(0, x, y)
}

// Test
assert(multTail(4, 3) == 12)
assert(multTail(0, 0) == 0)
assert(multTail(0, 1) == 0)
assert(multTail(1, 0) == 0)
assert(multTail(-3, 4) == -12)
assert(multTail(3, -4) == -12)
assert(multTail(-3, -3) == 9)
println("tests passed")