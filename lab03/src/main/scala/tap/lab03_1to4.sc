//  #1
val aList:List[Int] = List(1, 2, 3)
val bList=List("edom", "odsoft", "tap")
val cList=List('a', 'b')
val dList=List(true, false)
val e=5.6
val fList = List(1.0, 2, 3)
val g='i'

//> a)
bList.flatten
//> b)
bList.map(elem => elem.length)
//> c)
bList.flatten:::aList
//> d)
e::dList
//> e)
fList:::List(g.toString)

//  #2 List(1, 3, 5, 7, 9)
//> a)
List.range(1, 10).filter(x => x % 2 != 0)
//> b)
List.tabulate(5)(x => x + x+1)

//  #3

val l = List("Maria", "Ana", "Joana", "Julia", "Paulo", "José")

//> a)
l.filter(x => x.matches("^Jo.*"))
l.filter(x => x.matches("Joana|José"))
//> b)
for elem <- l if elem.matches("Joana|José") yield elem

//  #4

val x = (elem:Int) => if (elem <0) -elem else elem
List(1, 2, 3, -1, -2, -3, 0).map(x)
//> Result: List[Int] = List(1, 2, 3, 1, 2, 3, 0)
