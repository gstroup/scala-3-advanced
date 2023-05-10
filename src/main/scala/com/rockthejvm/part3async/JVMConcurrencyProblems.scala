package com.rockthejvm.part3async

object JVMConcurrencyProblems {

  def runInParallel(): Unit = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })

    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x)  // race condition.  no guarantee on which thread finishes first.
  }

  case class BankAccount(var amount: Int)

  def buy(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    // this line actually involves 3 steps: read, compute, write.
    bankAccount.amount -= price
  }

  def buySafe(bankAccount: BankAccount, thing: String, price: Int): Unit = {
    bankAccount.synchronized { // synchronized does not allow multiple threads to run this section at the same time.
      bankAccount.amount -= price // critical section
    }  // we've made this operation atomic
  }

  /**
   * Example race condition:
   * thread1 (shoes)
   *  - reads amount 50000
   *  - compute result 50000-3000 = 47000
   * thread2 ( phone)
   *  - reads amount 50000
   *  - compute result 50000-4000 = 46000
   * thread1 (shoes)
   *  - write amount 47000
   * thread2
   *  - write amount 46000
   */
  def demoBankingProblem(): Unit = {
    (1 to 10000).foreach { _ =>
      val account = BankAccount(50000)
      val thread1 = new Thread(() => buySafe(account, "shoes", 3000))
      val thread2 = new Thread(() => buySafe(account, "phone", 4000))
      thread1.start()
      thread2.start()
      thread1.join()
      thread2.join()
      if (account.amount != 43000) println(s"Fail!  incorrect amount: ${account.amount}")
    }
  }

  // 1 - create "inception threads"
  //  thread1 -> thread2 -> thread3... each prints "hello from thread $i".
  //  print all in reverse order
//  def inceptionThreads(): Unit = {  // this is my code. I forgot to add the .join().  now it works.
//    def helloThread(i: Int): Int = {
//      val j = i - 1
//      if (j <= 0) j
//      else {
//        val aThread = new Thread(() => println(s"Hello from thread $j"))
//        aThread.start()
//        aThread.join()
//        helloThread(j)
//      }
//    }
//    helloThread(50)
//  }
  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = {
    new Thread(() => {
      if (i < maxThreads) {
        val newThread = inceptionThreads(maxThreads, i + 1)
        newThread.start()
        newThread.join()
      }
      println(s"Hell from thread $i")
    })
  }

  /**
   * 2 - what's the max/min value of x
   *  max val = 100 - each thread increases x by 1
   *  min value = 1
   *    all threads read x = 0 at the same time
   *    all threads in parallel compute 0 + 1 = 1
   *    all threads try to write x = 1
   */
  def minMaxX(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
    threads.foreach(_.start())
  }

  /**
   * 3 - sleep fallacy
   * here's a possible nasty condition:
   *
   * main thread:
   *  message = "Scala sucks"
   *  awesomeThread.start()
   *  sleep(1001) - yields execution
   * awesomeThread:
   *  sleep(1000) - yields execution
   * OS gives the CPU to some important thread, takes > 2 s
   * OS gives the CPU back to the main thread
   * main thread:
   *  println(message) // "Scala sucks"
   * awesome thread:
   *  message = "Scala is awesome"
    */

  def demoSleepFallacy(): Unit = {
    var message = ""
    val awesomeThread = new Thread(() => {
      Thread.sleep(1000)
      message = "Scala is cool"  // this prints almost always, but it's not guaranteed.
    })
    message = "Scala sucks"
    awesomeThread.start()
    Thread.sleep(1001)
    // solution: join worker thread
    awesomeThread.join()
    println(message)
  }

  def main(args: Array[String]): Unit = {
//    runInParallel()
//    demoBankingProblem()
//    demoSleepFallacy()
    inceptionThreads(50)
  }
}
