package com.rockthejvm.part4context

// "given" and "using" are called "contextual abstractions"

object Givens {

  // list sorting
  val aList = List(4,2,3,1)
  val orderedList = aList.sorted
  // how does this magic sorting work?

  // here we use our own ordering
//  val descendingOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _)
//  val inverseOrderedList = aList.sorted(descendingOrdering)
  // or...
  given descendingOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _) // this affects the "orderedList" above too.
  val inverseOrderedList = aList.sorted

  // custom sorting
  case class Person(name: String, age: Int)
  val people = List(Person("Alice", 29), Person("Sarah", 34), Person("Jim", 23))
  given personOrdering: Ordering[Person] = new Ordering[Person] {
    override def compare(x: Person, y: Person): Int = x.name.compareTo(y.name)
  }
  val sortedPeople = people.sorted // (personOrdering) is automatically passed in by the compiler

  object PersonAltSyntax {
    // we can't define 2 givens with the same name, so we are "hiding" one inside this object.
    given personOrdering: Ordering[Person] with {  // same as personOrdering above
      override def compare(x: Person, y: Person): Int = x.name.compareTo(y.name)
    }
  }

  // using clauses - "using" tells the compiler that the function requires a "given"  <<--********************************
  trait Combinator[A] {
    def combine(x: A, y: A): A
  }

  def combineAll[A](list: List[A])(using combinator: Combinator[A]): A = {
    list.reduce(combinator.combine)
  }

  /**
   * combineAll(List(1,2,3,4))
   * combineAll(people)
   */
//  val firstSum = combineAll(List(1,2,3,4)) // won't compile without a given
  given intCombinator: Combinator[Int] with {
    override def combine(x: Int, y: Int) = x + y
  }
  val firstSum = combineAll(List(1,2,3,4)) // (intCombinator) is passed in automatically.
  //val combineAllPeople = combineAll(people) // does not compile.  needs a Combinator of type Person in scope.

  // context bound
  def combineInGroupsOf3[A](list: List[A])(using Combinator[A]): List[A] =
    list.grouped(3).map(group => combineAll(group)).toList
    // "A : Combinator" means a given Combinator[A] is in scope
  def combineInGroupsOf3_v2[A : Combinator](list: List[A]): List[A] =
    list.grouped(3).map(group => combineAll(group)).toList

  // synthesize new given instance based on existing one
  given listOrdering(using intOrdering: Ordering[Int]): Ordering[List[Int]] with {
    override def compare(x: List[Int], y: List[Int]) = x.sum - y.sum
  }
  val listOfLists = List(List(1,2), List(1,1), List(3,4,5))
  val nestedListsOrdered = listOfLists.sorted
  // works with generics too!
  given listOrderingBasedOnCombinator[A](using ord: Ordering[A])(using combinator: Combinator[A]): Ordering[List[A]] with {
    override def compare(x: List[A], y: List[A]): Int =
      ord.compare(combineAll(x), combineAll(y))
  }

  // pass a regular value instead of a given
  val myCombinator = new Combinator[Int] {
    override def combine(x: Int, y: Int): Int = x * y
  }
  val listProduct = combineAll(List(1,2,3,4))(using myCombinator)  // "using" has a different meaning here.

  /**
   * Exercises
   * 1 - create a given or ordering Option[A] if you can order A.
   */
  // 1
  //given optionOrdering[A](using normalOrdering: Ordering[A]): Ordering[Option[A]] with {
  given optionOrdering[A : Ordering]: Ordering[Option[A]] with {
    override def compare(x: Option[A], y: Option[A]): Int = (x, y) match {
      case (None, None) => 0
      case (None, _) => -1
      case (_, None) => 1
//      case (Some(a), Some(b)) => fetchGivenValue[Ordering[A]].compare(a, b)
      case (Some(a), Some(b)) => summon[Ordering[A]].compare(a, b)
    }
  }

  // 2 - create a summoning method that fetches the given value of your particular type
  def fetchGivenValue[A](using theValue: A): A = theValue
  // fetchGivenValue is called "summon" in the standard lib

  def main(args: Array[String]): Unit = {
    println(orderedList)
    println(inverseOrderedList)
    println(sortedPeople)
//    println(firstSum)
    println(List(Option(1), Option.empty[Int], Option(3), Option(-1000)).sorted)
  }
}
