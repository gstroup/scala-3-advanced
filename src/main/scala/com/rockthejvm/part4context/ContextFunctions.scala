package com.rockthejvm.part4context

import scala.concurrent.{ExecutionContext, Future}

object ContextFunctions {

  // Lambdas can take contextual arguments in scala 3
  val aList = List(1,2,3,4)
  val sortedList = aList.sorted

  // defs can take using clauses
  def methodWithoutContextArgs(nonContextArg: Int)(nonContextArg2: String): String = ???
  def methodWithContextArgs(nonContextArg: Int)(using nonContextArg2: String): String = ???

  // eta-expansion converts methods to function values.
  val functionWithoutContextArgs = methodWithoutContextArgs
//  val func2 = methodWithContextArgs   // does not compile

  // "context function":
  val functionWithContextArgs: Int => String ?=> String = methodWithContextArgs
  // the "?=>" is a new notation in scala 3.  it tells the compiler that we're using a given.
  val someResult = functionWithContextArgs(2)(using "a string")

  /**
   * Why do we need this?
   * - to convert methods with using clauses to function values
   *  - then we can pass them around
   * - HOF with function values taking given instances as args
   * - async issues with ExecutionContext
   */
  // execution context problem here
//  val incrementAsync: Int => Future[Int] = x => Future(x + 1)  // doesn't work without ExecutionContext in scope
  val incrementAsync: ExecutionContext ?=> Int => Future[Int] = x => Future(x + 1)

  def main(args: Array[String]): Unit = {

  }
}
