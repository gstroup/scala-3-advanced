package com.rockthejvm.part4context

object OrganizingCAs {

  val aList = List(2,3,1,4)
  val orderedList = aList.sorted

  // how does compiler search for contextual abstractions / implicits?
  // 1 - local scope
  given reverseOrdering: Ordering[Int] with {
    override def compare(x: Int, y: Int): Int = y - x // (4,3,2,1)
  }

  // 2 - imported scope
  case class Person(name: String, age: Int)
  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 67)
  )
  object PersonGivens {
    given ageOrdering: Ordering[Person] with
      override def compare(x: Person, y: Person): Int = y.age - x.age
  }
//  val sortedPersons = persons.sorted // compile error
  // a - import explicitly
  // import PersonGivens.ageOrdering

  // b - import just for one particular type:
  // import PersonGivens.{given Ordering[Person]}  // handy if you don't know the name!

  // c - import all givens from the PersonGivens
  // import PersonGivens.given
  // warning: "import PersonGivens.*" does NOT import given instances!

  // 3 - companion objects of all types involved in the method signature
  /**
   * compiler will check these object's companions:
   * - Ordering
   * - List
   * - Person
   */
  // def sorted[B >: A](using ord: Ordering[B]): List[B]

  object Person {
    given byNameOrdering: Ordering[Person] with
      override def compare(x: Person, y: Person): Int = x.name.compareTo(y.name)

    extension(p: Person)
      def greet(): String = s"Hi, my name is ${p.name}"
  }

  val sortedPersons = persons.sorted

  /**
   * Good practices:
   * 1. Put the most commonly used "default" given in the companion object of the type
   * 2. if there are many givens, and ONE is dominant, put that in the companion.  put the rest
   *    in separate objects, and import explicitly
   * 3. if there are many givens, and none is dominant, put them in separate objects, and import explicitly
   */

    // All the principles above apply to extension methods as well.

  // Exercises
  //  ordering by total price descending is dominant.
  //   others are order by unit count descending, and unit price ascending.
  case class Purchase(nUnits: Int, unitPrice: Double)  // never store money as a Double in real life!!

  object Purchase {
    given totalPriceOrdering: Ordering[Purchase] with
      override def compare(x: Purchase, y: Purchase): Int =
        (y.nUnits * y.unitPrice).compareTo((x.nUnits * x.unitPrice))
  }

  object UnitCountOrdering {
    given countDescOrdering: Ordering[Purchase] with
      override def compare(x: Purchase, y: Purchase): Int = y.nUnits.compareTo(x.nUnits)
  }

  object UnitPriceOrdering {
    given priceAscendingOrdering: Ordering[Purchase] with
      override def compare(x: Purchase, y: Purchase): Int = x.unitPrice.compareTo(y.unitPrice)
  }

  def main(args: Array[String]): Unit = {
    println(orderedList)
    println(sortedPersons)
    println(Person("Greg", 47).greet())
  }
}
