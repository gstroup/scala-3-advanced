package com.rockthejvm.practice

import scala.annotation.tailrec

// all sets return boolean
abstract class FSet[A] extends (A => Boolean) {
  def contains(elem:A): Boolean
  def apply(elem: A): Boolean = contains(elem)

  infix def +(elem: A): FSet[A]
  infix def ++(set2: FSet[A]): FSet[A]

  def map[B](f: A => B): FSet[B]
  def flatMap[B](f: A => FSet[B]): FSet[B]
  def filter(predicate: A => Boolean): FSet[A]
  def foreach(f: A => Unit): Unit

  // "level 2"
  infix def -(elem: A): FSet[A]
  infix def --(anotherSet: FSet[A]): FSet[A]
  infix def &(anotherSet: FSet[A]): FSet[A]

  // negation = all the elements EXCEPT elements in this set.
  def unary_! : FSet[A] = new PBSet(x => !contains(x))
}

// PropertyBasedSet - very powerful and general, but the downside is you can't iterate.
case class PBSet[A](property: A => Boolean) extends FSet[A] {
  def contains(elem: A): Boolean = property(elem)

  infix def +(elem: A): FSet[A] =
    new PBSet(x => x == elem || property(x))

  infix def ++(set2: FSet[A]): FSet[A] =
    new PBSet(x => property(x) || set2(x))

  def map[B](f: A => B): FSet[B] = politelyFail()

  def flatMap[B](f: A => FSet[B]): FSet[B] = politelyFail()

  def filter(predicate: A => Boolean): FSet[A] = {
    new PBSet(x => property(x) && predicate(x))
  }

  def foreach(f: A => Unit): Unit = politelyFail()

  // "level 2"
  infix def -(elem: A): FSet[A] = filter(x => x != elem)

  infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet)

  infix def &(anotherSet: FSet[A]): FSet[A] =
    filter(anotherSet)

  private def politelyFail() = throw new RuntimeException("don't know how to do this...")
}

case class Empty[A]() extends FSet[A] {  // PBSet(x => false)
  override def contains(elem: A): Boolean = false
  override infix def +(elem: A): FSet[A] = Cons(elem, this)
  override infix def ++(set2: FSet[A]): FSet[A] = set2

  override def map[B](f: A => B): FSet[B] = Empty()
  override def flatMap[B](f: A => FSet[B]): FSet[B] = Empty()
  override def filter(predicate: A => Boolean): FSet[A] = this
  override def foreach(f: A => Unit): Unit = ()

  infix def -(elem: A): FSet[A] = this
  infix def --(anotherSet: FSet[A]): FSet[A] = this
  infix def &(anotherSet: FSet[A]): FSet[A] = this
}

case class Cons[A](head: A, tail: FSet[A]) extends FSet[A] {
  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)

  override infix def +(elem: A): FSet[A] =  {
    if (contains(elem)) this // prevent dupes !!
    else Cons(elem, this)
  }

  override infix def ++(set2: FSet[A]): FSet[A] = tail ++ set2 + head

  override def map[B](f: A => B): FSet[B] = tail.map(f) + f(head)

  override def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)

  override def filter(predicate: A => Boolean): FSet[A] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  infix def -(elem: A): FSet[A] = {
    if (head == elem) tail
    else (tail - elem + head)
  }

  infix def --(anotherSet: FSet[A]): FSet[A] = filter(!anotherSet(_))

  // & is intersection of the sets.  intersection = filtering
  infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet)  //using set as a function to filter.  cool.

}

object FSet {
  def apply[A](values: A*): FSet[A] = {
    // the values vararg is actually a sequence
    @tailrec
    def buildSet(valuesSeq: Seq[A], acc: FSet[A]): FSet[A] =
      if (valuesSeq.isEmpty) acc
      else buildSet(valuesSeq.tail, acc + valuesSeq.head)

    buildSet(values, Empty())
  }
}

object FunctionalSetPlayground {

//  val aSet: Set

  def main(args: Array[String]): Unit = {
    val first5 = FSet(1,2,3,4,5)
    val someNumbers = FSet(4,5,6,7,8)
    println(first5.contains(5)) // true
    println(first5(6))          // false
    println((first5 + 10).contains(10)) // true
    println(first5.map(_ * 2).contains(10)) // true
    println(first5.map(_ % 2).contains(1)) // true
    println(first5.flatMap(x => FSet(x, x+1)).contains(7)) // false
    first5.flatMap(x => FSet(x, x+2)).foreach(println(_))

    /// scala standard library example:
//    val aSet = Set(1,2,3)
//    val aList = (1 to 10).toList
//    println(aList.filter(aSet))
    println((first5 - 3).contains(3)) // false
    println((first5 -- someNumbers).contains(4)) // false
    println((first5 & someNumbers).contains(4)) // true

    val naturals = new PBSet[Int](_ => true)
    println(naturals.contains(239482398)) // true
    println(!naturals.contains(2398)) // false
    println((!naturals + 1 + 2 + 3).contains(3)) // true
    println(!naturals.map(_ + 1)) // throws
  }
}
