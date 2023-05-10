package com.rockthejvm.practice

import scala.annotation.tailrec

// a lazily evaluted, potentially INFINITE linked list
abstract class LzList[A] {
  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  def #::(element: A): LzList[A] // prepending
  infix def ++(another: => LzList[A]): LzList[A]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): LzList[B]
  def flatMap[B](f: A => LzList[B]): LzList[B]
  def filter(predicate: A=> Boolean): LzList[A]
  def withFilter(predicate: A => Boolean): LzList[A] = filter(predicate)

  def take(n: Int): LzList[A] // takes the first n elements from the list
  def takeAsList(n: Int): List[A] = take(n).toList
  def toList: List[A] = {
    @tailrec
    def toListAux(remaining: LzList[A], acc: List[A]): List[A] = {
      if (remaining.isEmpty) acc.reverse
      else toListAux(remaining.tail, remaining.head :: acc)
    }
    toListAux(this, List())
  }
}
case class LzEmpty[A]() extends LzList[A] {
  def isEmpty: Boolean = true
  def head: A = throw new NoSuchElementException
  def tail: LzList[A] = throw new NoSuchElementException

  def #::(element: A): LzList[A] = new LzCons(element, this) // prepending
  infix def ++(another: => LzList[A]): LzList[A] = another

  def foreach(f: A => Unit): Unit = ()
  def map[B](f: A => B): LzList[B] = new LzEmpty[B]()
  def flatMap[B](f: A => LzList[B]): LzList[B] = LzEmpty() // same as "new LzEmpty[B]()
  def filter(predicate: A => Boolean): LzList[A] = this

  def take(n: Int): LzList[A] = {
    if (n == 0) this
    else throw new RuntimeException(s"cannot take $n elements from empty list")
  }
}

// case class cannot have args passed by name, since are defined as fields and they must be eagerly evaluated
// so we make this a regular class
class LzCons[A](h: => A, t: => LzList[A]) extends LzList[A] {
  def isEmpty: Boolean = false

  // hint: use call by need.
  override lazy val head: A = h
  override lazy val tail: LzList[A] = t

  def #::(element: A): LzList[A] = new LzCons[A](element, this)  // element and this are passed by name!
  infix def ++(another: => LzList[A]): LzList[A] = new LzCons(head, tail ++ another) // WATCH OUT

  def foreach(f: A => Unit): Unit = {
    def forEachTailRec(lzList: LzList[A]): Unit = {
      if (lzList.isEmpty) ()
      else {
        f(lzList.head)
        forEachTailRec(lzList.tail)
      }
    }
    forEachTailRec(this)
  }
  def map[B](f: A => B): LzList[B] = LzCons(f(head), tail.map(f))
  def flatMap[B](f: A => LzList[B]): LzList[B] = f(head) ++ tail.flatMap(f) // preserves lazy eval

  def filter(predicate: A => Boolean): LzList[A] = {
    if (predicate(head)) LzCons(head, tail.filter(predicate)) // preserves lazy eval
    else (tail.filter(predicate)) // WARNING
  }

  def take(n: Int): LzList[A] = {
    if (n <= 0) LzEmpty()
    else if (n == 1) new LzCons(head, LzEmpty())
    else new LzCons(head, tail.take(n-1))  // preserves lazy eval.
  }
}

object LzList {
  def empty[A]: LzList[A] = LzEmpty()

  def generate[A](start: A)(generator: A => A): LzList[A] = {
    new LzCons(start, LzList.generate(generator(start))(generator))
  }
  def from[A](list: List[A]): LzList[A] = {
    list.reverse.foldLeft(LzList.empty) { (currentLzList, newEl) =>
      new LzCons(newEl, currentLzList)
    }
  }

  def apply[A](values: A*) = LzList.from(values.toList)

  def fibonacci: LzList[BigInt] = {
    def fibo(first: BigInt, second: BigInt): LzList[BigInt] = {
      new LzCons[BigInt](first, fibo(second, first + second))
    }
    fibo(1, 2)
  }

  def eratosthenes: LzList[Int] = {
    def isPrime(n: Int) = {
      def isPrimeTailRec(potentialDivisor: Int): Boolean = {
        if (potentialDivisor < 2) true
        else if (n % potentialDivisor == 0) false
        else isPrimeTailRec(potentialDivisor - 1)
      }
      isPrimeTailRec(n / 2)
    }
    def sieve(numbers: LzList[Int]): LzList[Int] = {
      if (numbers.isEmpty) numbers
      else if (!isPrime(numbers.head)) sieve(numbers.tail)
      else LzCons[Int](numbers.head, sieve(numbers.tail.filter(_ % numbers.head != 0)))
    }
    sieve(LzList.generate(2)(_ + 1))
  }
}
object LzListPlayground {

  def main(args: Array[String]): Unit = {
    val naturals = LzList.generate(1)(n => n + 1) // Infinite list of natural numbers
    println(naturals.head)
    println(naturals.tail.head)
    println(naturals.tail.tail.head)

    val first50k = naturals.take(50000)
//    first50k.foreach(println)
    val first50kList = first50k.toList
//    println(first50kList)

    // classics
    println(naturals.map(_ * 2).takeAsList(100))
    println(naturals.flatMap(x => LzList(x, x+1)).takeAsList(100))
    println(naturals.filter(_ < 10).takeAsList(9))
//    println(naturals.filter(_ < 10).takeAsList(10))  // keeps searching the infinite list for the tenth element
    val combinationsLazy = for {
      num <- LzList(1,2,3)
      string <- LzList("black", "white")
    } yield s"$num-$string"
    println(combinationsLazy.toList)

    /**
     * 1. infinite lazy list of fibonacci numbers: 1, 2, 3, 5, 8, 13, 21, 24, ...
     * 2. infinite lazy list of prime numbers
     *  - filter with isPrime
     *  - Eratosthenes' sieve
     */
    val fibos = LzList.fibonacci
    println(fibos.takeAsList(100))

    val primes = LzList.eratosthenes
    println(primes.takeAsList(100))
  }
}
