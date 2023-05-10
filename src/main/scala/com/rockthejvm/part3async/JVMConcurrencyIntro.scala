package com.rockthejvm.part3async

import java.util.concurrent.Executors

object JVMConcurrencyIntro {

  def basicThreads(): Unit = {
    val runnable = new Runnable {
      override def run(): Unit = {
        println("waiting...")
        Thread.sleep(2000)
        println("running on a thread")
      }
    }

    // threads on the JVM
    val aThread = new Thread(runnable)
    aThread.start() // actually spawns a new JVM thread and runs the Runnable.
    // JVM thread has a 1-1 relationship with OS thread. (soon to change via Project Loom)
    aThread.join() // block until the thread finishes
  }

  // order of operations across threads is NOT guaranteed
  // different runs yield different results!
  def orderOfExecution(): Unit = {
    val threadHello = new Thread(() => (1 to 20).foreach(_ => println("hello")))
    val threadGoodbye = new Thread(() => (1 to 20).foreach(_ => println("goodbye")))
    threadHello.start()
    threadGoodbye.start()
  }

  // executors - thread pools
  def demoExecutors(): Unit = {
    val threadPool = Executors.newFixedThreadPool(4)
    // submit a computation
    threadPool.execute(() => println("running in thread pool"))

    threadPool.execute { () =>
      Thread.sleep(1000)
      println("done after one second")
    }
    threadPool.execute { () =>
      Thread.sleep(1000)
      println("almost done")
      Thread.sleep(1000)
      println("done after 2 seconds")
    }

    threadPool.shutdown()
//    threadPool.execute(() => println("this should NOT appear"))  // throws exception since pool is shut down.
  }

  def main(args: Array[String]): Unit = {
//    basicThreads()
//    orderOfExecution()
    demoExecutors()
  }
}
