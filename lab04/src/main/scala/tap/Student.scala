package tap

final case class Student(firstName: String, lastName: String, age: Int):

  def this(name: String, age: Int) =
    this(name, "lastName", age)

object Student {
  private def unsafeStudent(firstName: String, lastName: String, age: Int): Student = Student(firstName, lastName, age)

  def from(firstName: String, lastName: String, age: Int): Option[Student] =
    if (firstName.matches("[a-zA-Z]") && lastName.matches("[a-zA-Z]") && age > 0)
      Some(unsafeStudent(firstName, lastName, age)) else None
    
  def from(name: String, age: Int): Option[Student] =
    if (name.matches("[a-zA-Z ]") && age > 0)
      Some(unsafeStudent(name, "lastName", age)) else None
}