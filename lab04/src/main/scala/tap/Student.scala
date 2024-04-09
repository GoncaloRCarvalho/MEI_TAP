package tap

final case class Student(firstName: String, lastName: String, age: Int)

object Student {
  private def unsafeStudent(firstName: String, lastName: String, age: Int): Student = Student(firstName, lastName, age)

  private val isValidName: String => Boolean = {_.matches("^[a-zA-Z]*$")}
  private val isValidFullName: String => Boolean = {_.matches("^[a-zA-Z ]*$")}
  private val isValidAge: Int => Boolean = {_ > 0}

  def from(firstName: String, lastName: String, age: Int): Option[Student] =
    if (isValidName(firstName) && isValidName(lastName) && isValidAge(age))
      Some(unsafeStudent(firstName, lastName, age)) else None

  def from(name: String, age: Int): Option[Student] =
    if (isValidFullName(name) && isValidAge(age))
      val nameArray = name.split(" ")
      val fn = nameArray.apply(0)
      val ln = nameArray.apply(nameArray.length - 1)
      Some(unsafeStudent(fn, ln, age)) else None
}