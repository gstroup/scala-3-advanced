package com.rockthejvm.part2afp

object CurryingPAFs {

  // currying - passing args one at a time
  val superAdder: Int => Int => Int =
    x => y => x + y

  val add3: Int => Int = superAdder(3)  // y => 3 + y
  val eight = add3(5) // 8
  val eight_v2 = superAdder(3)(5)

  // curried methods
  def curriedAdder(x: Int)(y: Int): Int = x + y

  // methods != function values in scala
  // methods are on an object.  function values are instances of FunctionN

  // converting methods to functions = eta-expansion.
  // call a curried function with only some of its args.
  //  this returns a "Partially Applied Function" or PAF
  val add4 = curriedAdder(4)  // compiler runs "eta-expansion" here
  val nine = add4(5) // 9

  def increment(x: Int): Int = x + 1
  val aList = List(1,2,3)
  val anIncrementedList = aList.map(increment) // eta-expansion

  // underscores are powerful.  you can define the shapes of lambdas obtained from methods.
  def concatenator(a: String, b: String, c: String): String = a + b + c
//  val conncatenatorFunc = concatenator
  val insertName = concatenator("Hello, my name is ", _: String, ". fun stuff.")
    // x => concatenator("...", x, "...")

  val gregsGreeting = insertName("Greg")

  val fillInTheBlanks = concatenator(_: String, " Greg ", _: String)

  /// EXERCISES
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedMethod(x: Int)(y: Int) = x + y

  val add7 = (x: Int) => simpleAddFunction(x, 7)
  val add7_2 = (x: Int) => simpleAddMethod(x, 7)
  val add7_3 = (x: Int) => curriedMethod(7)(x)
  val add7_4 = curriedMethod(7)
  val add7_6 = simpleAddMethod(_, 7)
  val add7_7 = simpleAddFunction.curried(7)

  ///
  val piWith2Dec = "%4.2f".format(Math.PI)

  def curriedFormatter(fmt: String)(x: Double) = fmt.format(x)
  val someDecimals = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)

  // methods vs functions + by-name vs 0-lambdas
  // syntax is similar:
  def byName(n: => Int) = n + 1
  def byLambda(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42

  byName(23) // ok
  byName(method) // eta-expansion?  no, not here.  "method" is invoked
  byName(parenMethod()) // 42
//  byName(parenMethod) // not cool.  doesn't compile.  used to work in scala 2.
  byName((() => 42)())  // ok
//  byName(() => 42) // not ok.

//  byLambda(23) // obviously not.
//  byLambda(method) // also not ok.  eta-expansion is NOT possible
  byLambda(parenMethod) // eta-expansion is done.  we need the empty argument list.
  byLambda(() => 42)
  byLambda(() => parenMethod())

  def main(args: Array[String]): Unit = {
    println(gregsGreeting)
    println(fillInTheBlanks("Hey", "what's up"))
    println(curriedFormatter("%4.2f")(Math.PI))
    println(someDecimals.map(curriedFormatter("%4.3f")))
  }
}
