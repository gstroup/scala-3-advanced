package com.rockthejvm.part2afp

object PartialFunctions {

  val aFunction: Int => Int = x => x + 1

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new RuntimeException("bad")

  // this function is just a match for 1,2,5
  //  this is called a partial function, since it only applies to some ints, not all.
  val aFussyFunction_2 = (x: Int) => x match {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  val aPartialFunction: PartialFunction[Int, Int] = { // same as above
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  val canCallOn37 = aPartialFunction.isDefinedAt(37) // false
  val liftedPartialFunc = aPartialFunction.lift // makes pf into a total function that returns Option
  val anotherPF: PartialFunction[Int, Int] = {
    case 45 => 86
  }
  val pfChain = aPartialFunction.orElse[Int, Int](anotherPF) // runs both PFs in sequence

  // HOFs accept PFs as arguments.  (PartialFunctions extend FunctionN types)
  val aList = List(1,2,3,4)
  val aChangedList = aList.map(x => x match {
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
    case _ => 0
  })

  val aChangedList2 = aList.map({
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
    case _ => 0
  })

  val aChangedList3 = aList.map {  // no parens
    case 1 => 4
    case 2 => 3
    case 3 => 45
    case 4 => 67
    case _ => 0
  }

  case class Person(name: String, age: Int)
  val somePeople = List(
    Person("Alice", 3),
    Person("Bob", 5),
    Person("Jane", 4)
  )

//  val kidsGrowing = somePeople.map(p => Person(p.name, p.age + 1)) // standard way
// or use partial function like this:
  val kidsGrowing = somePeople.map {
    case Person(name, age) => Person(name, age + 1)
  }

  def main(args: Array[String]): Unit = {
    println(aFussyFunction_2(2))
    println(aPartialFunction(1))
//    println(aPartialFunction(77)) // throws match error
    println(liftedPartialFunc(5)) // Some(999)
    println(liftedPartialFunc(3838)) // None
    println(pfChain(45))
  }
}
