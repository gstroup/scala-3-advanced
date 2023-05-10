package com.rockthejvm.part1as

object AdvancedPatternMatching {

  /**
   * pattern matching:
   * constants, objects, wildcards, etc, etc.
   *
   * we'll look at matching on our own class
   */

  class Person(val name: String, val age: Int)

  object Person {
    //signature of this method is important!  now we can use it in pattern matching.
    def unapply(person: Person): Option[(String, Int)] = { // person match {case Person(string, int) ...}
      if (person.age < 21) None
      else (Some((person.name, person.age)))
    }

    def unapply(age: Int): Option[String] = { // int match { case Person(string) ...}
      if (age < 21) Some("minor")
      else Some("legally alllowed to drink")
    }
  }

  val daniel = new Person("Daniel", 102)
  val danielPm = daniel match { // Person.unapply(daniel) --> Option((n, a))
    case Person(n, a) => s"Hi, I'm $n, and "
  }

  val danielsLegalStatus = daniel.age match {  // match on int, but Person is where we defined unapply
    case Person(status) => s"Daniel's drinking status is $status"
  }

  // boolean patterns
  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  val n: Int = 8
  val mathProperty = n match {
    case even() => "it's even"
    case singleDigit() => "one digit"
    case _ => "nothing special"
  }

  // infix patterns - work well with collections
  infix case class Or[A, B](a: A, b: B) // case class companion object has the unapply method.
  val anEither = Or(2, "two")
  val humanDescriptionEither = anEither match {
//    case Or(number, string) => s"$number is written as $string"
    case number Or string => s"$number is written as $string"
  }

  val aList = List(1,2,3)
  val listPM = aList match {
    case 1 :: rest => "a list starting with 1"
    case _ => "another list"
  }

  // decomposing sequences
  val vararg = aList match {
    case List(1, _*) => "list starting with 1"
    case _ => "another list"
  }

  abstract class MyList[A] {
    def head: A = throw new NoSuchElementException()
    def tail: MyList[A] = throw new NoSuchElementException()
  }

  case class Empty[A]() extends MyList[A]
  case class Cons[A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] = {
      if (list == Empty()) Some(Seq.empty)
      else unapplySeq(list.tail).map(restOfSeq => list.head +: restOfSeq)
    }
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty())))
  val varargCustom = myList match {
    case MyList(1, _*) => "starts with 1"
    case _ => "other list"
  }

  // custom return type for unapply - probably never will need this in real live!
  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper { // type returned by unapply must have isEmpty and get. 
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false
      override def get: String = person.name
    }
  }

  val weirdPersonPM = daniel match {
    case PersonWrapper(name) => s"my name is $name"
  }

  def main(args: Array[String]): Unit = {
    println(danielPm)
    println(danielsLegalStatus)
    println(mathProperty)
    println(humanDescriptionEither)
    println(varargCustom)
  }
}
