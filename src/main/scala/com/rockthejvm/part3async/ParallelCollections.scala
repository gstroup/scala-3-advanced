package com.rockthejvm.part3async

import scala.collection.parallel.*
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParVector

object ParallelCollections {

  // standard collection
  val aList = (1 to 1000000).toList
  val anIncrementedList = aList.map(_ + 1)
  // parallel collection (see import above)
  val parList: ParSeq[Int] = aList.par // .par comes from CollectionConverters package
  val aParallelizedIncrementedList = parList.map(_ + 1) // also can call flatMap, filter, foreach, etc
  /*
  Applicable for
  - Seq
  - Vector
  - Arrays
  - Maps
  - Sets
  Use-case: faster processing!
  */

  // build a parallel collection yourself:
  val parVector = ParVector[Int](1,2,3,4,5,6)

  // pass expression by name
  def measure[A](expression: => A): Long = {
    val time = System.currentTimeMillis()
    expression // force evaluation
    System.currentTimeMillis() - time
  }

  def compareListTransformation(): Unit = {
    val list = (1 to 30000000).toList
    println("list is created")

    val serialTime = measure(list.map(_ + 1))
    println(s"serial time: $serialTime")

    val parallelTime = measure(list.par.map(_ + 1))
    println(s"parallel time: $parallelTime") // this also includes time to transform the list to a par list.
    // speed improvement increases with the size of the list
  }

  def demoUndefinedOrder(): Unit = {
    val list = (1 to 1000).toList
    val reduction = list.reduce(_ - _)  // using subtraction is risky!  it's non-associative.  order matters.
    // 1 - 2 - 3 = - 4
    // 1 - (2 - 3) = 2
    val parallelReduction = list.par.reduce(_ - _)
    println(s"sequential reduction: $reduction")
    println(s"parallel reduction: $parallelReduction")  // yields a different result each time.
  }

  def demoDefinedOrder(): Unit = {  // addition / concatenation is associative.  result is deterministic.  no worries.
    val strings = "I Love parallel collections but I must be careful".split(" ").toList
    val concatenation = strings.reduce(_ + " " + _)
    val parallelConcat = strings.par.reduce(_ + " " + _)
    println(s"serial concat: $concatenation")
    println(s"parallel concat: $parallelConcat")
  }

  def demoRaceConditions(): Unit = {
    var sum = 0
    (1 to 1000).toList.par.foreach(elem => sum += elem) // ugly imperative style code.
    println(s"sum: $sum")  // sum is not deterministic!  different with each run.
  }

  def main(args: Array[String]): Unit = {
//    compareListTransformation()
//    demoUndefinedOrder()
//    demoDefinedOrder()
    demoRaceConditions()
  }
}
