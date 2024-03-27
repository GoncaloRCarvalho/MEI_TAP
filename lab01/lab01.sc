//======Condtional Expressions======
def lessThan(a: Int, b: Int) =
  a < b

lessThan(1,2)
lessThan(3,2)
lessThan(2,2)

//==================================

def and(a: Boolean, b: Boolean) =
  if(a) a==b else false

def or(a: Boolean, b: Boolean) =
  if(a) a else if(b) b else false

assert(and(true, true)==true)
assert(and(true, false)==false)
assert(and(false, true)==false)
assert(and(false, false)==false)
assert(or(false, false)==false)
assert(or(true, true)==true)
assert(or(true, false)==true)
assert(or(false, true)==true)
println("tests passed")

//============Recursion============
