package com.rockthejvm.part2afp

import scala.annotation.targetName

object Monads {

  def listStory(): Unit = {
    val aList = List(1,2,3)
    val listMultiply = for {
      x <- List(1,2,3)
      y <- List(4,5,6)
    } yield x * y
    // same for-comprehension written with map & flatmap
    val listMultiply2 = List(1,2,3).flatMap(x => List(4,5,6).map(y => x * y))
//    println(listMultiply)
//    println(listMultiply2)
    val f = (x: Int) => List(x, x + 1)
    val g = (x: Int) => List(x, 2 * x)
    val pure = (x: Int) => List(x)  // same as the list "constructor"

    // properties of lists:
    // prop 1: left identity
    val leftIdentity = pure(42).flatMap(f) == f(42) // for every x and every f

    // prop 2: right identity
    val rightIdentity = aList.flatMap(pure) == aList // for every list

    // prop 3: associativity - guarantees order of application of the functions
    val associativity = aList.flatMap(f).flatMap(g) == aList.flatMap(x => f(x).flatMap(g))
    // [1,2,2,4, 2,4,3,6, 3,6,4,8]
  }

  def optionStory(): Unit = {
    val anOption = Option(42)
    val optionString = for {
      lang <- Option("Scala")
      ver <- Option(3)
    } yield s"$lang-$ver"

    val optionString2 = Option("Scala").flatMap(lang => Option(3).map(ver => s"$lang-$ver"))

    val f = (x: Int) => Option(x + 1)
    val g = (x: Int) => Option(x * 2)
    val pure = (x: Int) => Option(x) // same as Option "constructur"

    // properties of Option
    // prop 1: left identity
    val leftIdentity = pure(42).flatMap(f) == f(42) // for any x and y
    // prop 2: right identity
    val rightIdentity = anOption.flatMap(pure) == anOption // for any Option
    // prop 3: associativity
    val associativity = anOption.flatMap(f).flatMap(g) == anOption.flatMap(x => f(x).flatMap(g))
  }

  // things that have these 3 properties are called MONADS.
  // MONADS = chain dependent computations.

  // let's test if something is a monad
  // answer: YES, it is a Monad!
  // interpretation: ANY computation that might perform side effects.
  //  we've separated description of the function from execution
  //  this is a simplified version of the IO in ZIO and cats-effect
  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] =
      IO(() => f(unsafeRun()))

    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(unsafeRun()).unsafeRun())
  }

  object IO {
    @targetName("pure")
    def apply[A](value: => A): IO[A] =
      new IO(() => value)
  }

  def possiblyMonadStory(): Unit = {
    val aPossibleMonad = IO(33)
    val f = (x: Int) => IO(x + 1)
    val g = (x: Int) => IO(2 * x)
    val pure = (x: Int) => IO(x)

    val leftIdentity = pure(42).flatMap(f) == f(42)
    println(leftIdentity)
    val rightIdentity = aPossibleMonad.flatMap(pure) == aPossibleMonad
    println(rightIdentity)
    val associativity = aPossibleMonad.flatMap(f).flatMap(g) == aPossibleMonad.flatMap(x => f(x).flatMap(g))
    println(associativity)
    // all 3 return false, so we could conclude that this is not a monad. HOWEVER...
    println(IO(3) == IO(3))
    // ^^ false negative.
    // these tests are not correct.

    // real tests: values produced AND side effect ordering
    val leftIdentity2 = pure(42).flatMap(f).unsafeRun() == f(42).unsafeRun()
    val rightIdentity2 = aPossibleMonad.flatMap(pure).unsafeRun() == aPossibleMonad.unsafeRun()
    val associativity2 = aPossibleMonad.flatMap(f).flatMap(g).unsafeRun() == aPossibleMonad.flatMap(x => f(x).flatMap(g)).unsafeRun()
    println(leftIdentity2)
    println(rightIdentity2)
    println(associativity2)

    val fs = (x: Int) => IO {
      println("incrementing")
      x + 1
    }
    val gs = (x: Int) => IO {
      println("doubling")
      x * 2
    }
    val associativity3 = aPossibleMonad.flatMap(fs).flatMap(gs).unsafeRun() == aPossibleMonad.flatMap(x => fs(x).flatMap(gs)).unsafeRun()
  }

  // "real life" example
  def possiblyMonadExample(): Unit = {
    val aPossibleMonad = IO {
      println("my first possibly monad")
      // ...
      42
    }
    val anotherPM = IO {
      println("my second pm")
      "Scala"
    }
//    val aResult = aPossibleMonad.unsafeRun()
//    println(aResult)
    val aForComp = for {  // these computations are described, but not executed
      num <- aPossibleMonad
      lang <- anotherPM
    } yield s"$num-$lang"
//    println(aForComp.unsafeRun())  // this finally executes the computations
  }

  def main(args: Array[String]): Unit = {
//    listStory()
    possiblyMonadStory()
//    possiblyMonadExample()
  }
}
