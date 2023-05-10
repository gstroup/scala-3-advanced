package com.rockthejvm.part2afp

object FunctionalCollections {

  // sets are functions.  A => Boolean
  val aSet: Set[String] = Set("I", "Love", "Scala")
  val setContainsScala = aSet("Scala") // true

  // other collections are too.
  // Seq is a PartialFunction[Int, A] => A
  // actually a partial function
  val aSeq : Seq[Int] = Seq(1,2,3,4)
  val anElement = aSeq(2) // returns 3
//  val aNonExistingElement = aSeq(100) // throws out of bounds exception.

  // Map[K, V] is a PartialFunction[K, V]
  val phonebook = Map(
    "Alice" -> 123456,
    "Bob" -> 456789
  )
  val alicesPhone = phonebook("Alice") // returns 123456
//  val myPhone = phonebook("Greg")  // throws exception
  
  def main(args: Array[String]): Unit = {
    println(alicesPhone)
  }
}
