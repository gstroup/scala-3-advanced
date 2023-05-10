package com.rockthejvm.part4context

object Implicits {
  // implicits are the "old" way of doing:
  // given/using
  // extension methods
  // implicit conversions
  // in scala 2.

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  // in scala 2, we used the word "implicit" here instead of "using":
//  def combineAll[A](list: List[A])(using semigroup: Semigroup[A]): A =
  def combineAll[A](list: List[A])(implicit semigroup: Semigroup[A]): A =
    list.reduce(semigroup.combine)

  // in scala 2, they used "implicit val" here instead of "given":
//  given intSemigroup: Semigroup[Int] with
  implicit val intSemigroup: Semigroup[Int] = new Semigroup[Int] {
    override def combine(x: Int, y: Int): Int = x + y
  }

  val sumOf10 = combineAll((1 to 10).toList)

  // extension methods in scala 3
//  extension (number: Int)
//    def isEven = number % 2 == 0
//  val is23even = 23.isEven
  // extension methods in scala 2 = implicit class
  implicit class MyRichInteger(number: Int) {
    def isEven = number % 2 == 0
  }
  val is23even = 23.isEven  // uses the new MyRichInteger

  // implicit conversions - these were dangerous in scala 2
  case class Person(name: String) {
    def greet(): String = s"Hi my name is $name"
  }
  // scala 2 implicit conversion:  SUPER DANGEROUS:
  implicit def string2Person(x: String): Person = Person(x)
  val danSaysHi = "Dan".greet() // string2Person("Dan").greet()
  // implicit def was used to synthesize NEW implicit values
  implicit def semigroupOfOption[A](implicit semigroup: Semigroup[A]): Semigroup[Option[A]] = new Semigroup[Option[A]] {
    override def combine(x: Option[A], y: Option[A]): Option[A] = for {
      valueX <- x
      valueY <- y
    } yield semigroup.combine(valueX, valueY)
  }
  // same in scala 3:
//  given semigroupOfOption[A](using semigroup: Semigroup[A]): Semigroup[Option[A]] with ... same body

  /**
   * why implicits are being phased out:
   * - the "implicit" keyword was overloaded with many different meanings
   * - conversions are easy to abuse
   * - implicits are very hard to track down while debugging!!
   *    (givens are slightly better since they are explicitly imported)
   *
   *  But they are still fully supported in scala 3 for now.
   */

  // organizing implicits should be done in the same way as organizing contextual abstractions.
  //  except for one thing:  import yourPackage.* // also imports implicits.

  def main(args: Array[String]): Unit = {
    println(sumOf10)
  }
}
