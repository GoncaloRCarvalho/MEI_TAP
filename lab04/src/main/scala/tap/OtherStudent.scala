package tap

import SimpleTypes.*

final case class OtherStudent(firstName: String, lastName: String, age: Int)

object OtherStudent:
  private def unsafeOtherStudent(firstName: String, lastName: String, age:Int): OtherStudent = OtherStudent(firstName, lastName, age)
  private val isValidName: String => Boolean = {_.matches("^[a-zA-Z]*$")}
  private val isValidFullName: String => Boolean = {_.matches("^[a-zA-Z ]*$")}
  private val isValidAge: Int => Boolean = {_ > 0}

  def from(firstName: String, lastName: String, age: Int): Option[OtherStudent] =
    if (isValidName(firstName) && isValidName(lastName) && isValidAge(age))
      Some(unsafeOtherStudent(firstName, lastName, age)) else None

  def from(name: String, age: Int): Option[OtherStudent] =
    if (isValidFullName(name) && isValidAge(age))
      val nameArray = name.split(" ")
      val fn = nameArray.apply(0)
      val ln = nameArray.apply(nameArray.length - 1)
      Some(unsafeOtherStudent(fn, ln, age)) else None
