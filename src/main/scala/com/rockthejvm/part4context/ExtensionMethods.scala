package com.rockthejvm.part4context

// we can enhance existing classes after they've been defined.
object ExtensionMethods {

  case class Person(name: String) {
    def greet: String = s"Hi my name is $name, nice to meet you!"
  }

  extension (string: String)  // extending the String type
    def greetAsPerson: String = Person(string).greet  // with this new method

  val gregGreeting = "Greg".greetAsPerson

  // generic extension methods
  extension [A](list: List[A])  {
    def ends: (A, A) = (list.head, list.last)
  }
  val aList = List(1,2,3,4)
  val firstLast = aList.ends
  // why extension methods?
  // - To make APIs very expressive
  // - To enhance CERTAIN types with new capabilities
  // => powerful code & libraries like ZIO and cats effect.
  trait Combinator[A] {  // in math terms, this combinator is called a "Semigroup"
    def combine(x: A, y: A): A
  }
  extension [A](list: List[A])
    def combineAll(using combinator: Combinator[A]): A =
      list.reduce(combinator.combine)
  given intCombinator: Combinator[Int] with {
    override def combine(x: Int, y: Int): Int = x + y
  }
  val firstSum = aList.combineAll // 10
//  List("a", "b", "c").combineAll  // does not compile. no combinator for String in scope

  // grouping extensions
  object groupedExtensions {
    extension[A] (list: List[A]) {
      def ends: (A, A) = (list.head, list.last)
      def combineAll (using combinator: Combinator[A] ): A =
      list.reduce (combinator.combine)
    }
  }

  // call extension methods directly
  val firstLast2 = ends(aList) // same as aList.ends

  /**
   * Exercises
   * 1. add an isPrime method to the Int type: 7.isPrime
   * 2. using Tree, Leaf, Branch,
   *  add extensions to Tree:
   *  - map(f: A=> B): Tree[B]
   *  - forall(predicate: A => Boolean): Boolean
   *  - sum: returns sum of all elements of the tree[Int]
   */

  // 1
  extension (n: Int) {
    def isPrime = {
      def isPrimeTailRec(potentialDivisor: Int): Boolean = {
        if (potentialDivisor < 2) true
        else if (n % potentialDivisor == 0) false
        else isPrimeTailRec (potentialDivisor - 1)
      }
      isPrimeTailRec (n / 2)
    }
  }

  // 2
  // Tree, Leaf, Branch can not be changed for this exercise
  sealed abstract class Tree[A]
  case class Leaf[A](value: A)  extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  extension [A](tree: Tree[A]) {
    def map[B](f: A => B): Tree[B] = tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(l, r) => Branch(l.map(f), r.map(f))  // recursively call this map extension
    }

    def forall(predicate: A => Boolean): Boolean = tree match {
      case Leaf(value) => predicate(value)
      case Branch(left, right) => left.forall(predicate) && right.forall(predicate)
    }

    def combineAll(using combinator: Combinator[A]): A = tree match {
      case Leaf(value) => value
      case Branch(l, r) => combinator.combine(l.combineAll, r.combineAll)
    }
  }
  extension (tree: Tree[Int]) {
    def sum: Int = tree match {
      case Leaf(value) => value
      case Branch(l, r) => l.sum + r.sum
    }
  }

  def main(args: Array[String]): Unit = {
//    println(gregGreeting)
//    println(firstLast)
//    println(firstSum)
//    println(7.isPrime)
//    println(8.isPrime)
    val aTree: Tree[Int] = Branch(Branch(Leaf(3), Leaf(1)) ,Leaf(10))
    println(aTree.map(_ + 1))
    println(aTree.forall(_ % 2 == 0 )) // false
    println(aTree.sum)
    println(aTree.combineAll)  // we have a combinator[Int] in scope
  }
}
