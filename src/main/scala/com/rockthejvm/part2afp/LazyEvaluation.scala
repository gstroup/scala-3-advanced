package com.rockthejvm.part2afp

object LazyEvaluation {

  val x: Int = {
    println("Hello")  // this prints right away, when it is defined here.
    42
  }
  // lazy delays the evaluation of a value until the first use.
  lazy val y: Int = {
    println("Hello again")
    43
  }

  def byNameMethod(n: => Int): Int =  {
    n + n + n + 1
  }

  def retrieveMagicValue() = {
    println("waiting...")
    Thread.sleep(1000)
    42
  }

  def demoByName(): Unit = {
    println(byNameMethod(retrieveMagicValue()))
    // retrieveMagicValue() + retrieveMagicValue() + retrieveMagicValue() + ()
  }

  // call by need = call by name + lazy values
  def byNeedMethod(n: => Int): Int = {
    lazy val lazyN = n // memoization - argument only evaluated ONCE, and delayed
    lazyN + lazyN + lazyN + 1
  }

  def demoByNeed(): Unit = {
    println(byNeedMethod(retrieveMagicValue()))
  }

  // withFilter
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbersList = List(1, 25, 40, 5, 23)

  def demoFilter(): Unit = {
    val lt30 = numbersList.filter(lessThan30)
    val gt20 = lt30.filter(greaterThan20)
    println(gt20)
  }

  def demoWithFilter(): Unit = {  // withFilter uses lazy evaluation
    val lt30 = numbersList.withFilter(lessThan30)
    val gt20 = lt30.withFilter(greaterThan20)
    println(gt20.map(identity))  // identity is x => x
  }

  def demoForComp(): Unit = {
    val forComp = for {
      // lazy again.  essentially evaluates each n against both functions if needed.
      //    rather than demoFilter, which evaluates each against one function, then each against the 2nd func.
      n <- numbersList if lessThan30(n) && greaterThan20(n)
    } yield n
    println(forComp)
  }

  def main(args: Array[String]): Unit = {
//    println(x)
//    println(x)
//    println(y)

//    demoByName()
//    demoByNeed()

    demoForComp()
  }
}
