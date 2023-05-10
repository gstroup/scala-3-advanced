package com.rockthejvm.part4context

// special import:
import scala.language.implicitConversions

object ImplicitConversions {

  case class Person(name: String) {
    def greet(): String = s"Hi, I'm $name"
  }
  val greg = Person("Greg")
  val gregGreeting = greg.greet()

  given string2Person: Conversion[String, Person] with
    override def apply(x: String): Person = Person(x)  // could have more complex logic here if needed.

  // now the compiler can autobox the String as a Person
  val gregGreeting2 = "Greg".greet()

  def processPerson(person: Person): String =
    if (person.name.startsWith("J")) "OK"
    else "NOT OK"

  val isJaneOk = processPerson("Jane")
  // this works since we've imported the implicitConversions package
  //  AND we have the string2Person converter defined in this scope.



  def main(args: Array[String]): Unit = {

  }
}
