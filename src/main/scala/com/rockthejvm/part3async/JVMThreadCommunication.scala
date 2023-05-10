package com.rockthejvm.part3async

import scala.collection.mutable
import scala.util.Random

object JVMThreadCommunication {
  def main(args: Array[String]): Unit = {
    ProdConsV4.start(2, 4, 5)
  }
}

// problem: the producer / consumer problem

class SimpleContainer {
  private var value: Int = 0

  def isEmpty: Boolean = value == 0
  def set(newVal: Int): Unit = value = newVal
  def get: Int = {
    val result = value
    value = 0
    result
  }
}

// P/C part 1: one producer, one consumer
object ProdConsV1 {
  def start(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while (container.isEmpty) {  // this is called "busy waiting".  not cool.  this blocks the CPU.
        println("[consumer] container is empty")
      }
      println(s"[consumer] I have consumed a value: ${container.get}")
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      println(s"[producer] I produced the value $value")
      container.set(value)
    })

    consumer.start()
    producer.start()
  }
}

// wait + notify
object ProdConsV2 {
  def start(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      container.synchronized({ // block all other threads trying to "lock" the object
        // thread-safe code
        if (container.isEmpty)
          container.wait() // release the lock + suspend the thread.  not using CPU.  thread is "dormant"
        // reacquire the lock here
        // continue execution
        println(s"[consumer] I have consumed a value: ${container.get}")
      })
    })

    val producer = new Thread(() => {
      println("[producer] computing...")
      Thread.sleep(500)
      val value = 42
      container.synchronized({
        println(s"[producer] I produced the value $value")
        container.set(value)
        container.notify() // awaken ONE suspended thread on this object.
          // works in this example since we just have one consumer.
      })  // release the lock
    })

    consumer.start()  // consumer might not start before the producer here.
    producer.start()  // if producer notifies before consumer starts, consumer will wait forever.
  }
}

// a larger container
// producer -> [ queue ] -> consumer
object ProdConsV3 {
  def start(containerCapacity: Int): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]

    val consumer = new Thread(() => {
      val random = new Random(System.nanoTime())

      while (true) { // ugly code!  just for demo.
        buffer.synchronized({
          if (buffer.isEmpty) {
            println("[consumer] buffer empty.  waiting.")
            buffer.wait()
          }
          // buffer not empty
          val x = buffer.dequeue()
          println(s"[consumer] consumed a value: $x")

          buffer.notify()  // wake up the producer.  like asking for more elements.
        })
        Thread.sleep(random.nextInt(500))
      }
    })

    val producer = new Thread(() => {
      val random = new Random(System.nanoTime())
      var counter = 0

      while (true) {
        buffer.synchronized({
          if (buffer.size == containerCapacity) {
            println("[producer] buffer full.  waiting.")
            buffer.wait()
          }
          val newElement = counter
          counter += 1
          println(s"[producer] producing an element $newElement")
          buffer.enqueue(newElement)
          buffer.notify() // wakes up the consumer (if it's asleep).  like telling consumer to consume.
        })
        Thread.sleep(random.nextInt(500))
      }
    })

    consumer.start()
    producer.start()
  }
}

// large container with a single queue, and multiple producers / consumers
object ProdConsV4 {
  class Consumer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random(System.nanoTime())
      while(true) {
        buffer.synchronized {
          while (buffer.isEmpty) { // need a while here, not an if.
            // since we need to contantly check if the buffer is really empty.
            println(s"[consumer $id] buffer empty.  waiting.")
            buffer.wait()
          }
          val newVal = buffer.dequeue()
          println(s"[consumer $id] consumed $newVal")
          // notify producer

          //buffer.notify() // this could potentially awaken another consumer!
            // that consumer needs to check to make sure buffer is not empty before dequeueing
            // or one producer could notify another producer.
            // could lead to a deadlock.

          // use notifyAll() instead to notify all the threads
          buffer.notifyAll()
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  class Producer(id: Int, buffer: mutable.Queue[Int], capacity: Int) extends Thread {
    override def run(): Unit = {
      val random = new Random(System.nanoTime())
      var currentCount = 0

      while(true) {
        buffer.synchronized {
          while (buffer.size == capacity) {  // need while here too
            println(s"[producer $id] buffer is full. waiting.")
            buffer.wait()
          }
          println(s"[producer $id] producing $currentCount")
          buffer.enqueue(currentCount)

          buffer.notifyAll()

          currentCount += 1
        }
        Thread.sleep(random.nextInt(500))
      }
    }
  }

  def start(nProducers: Int, nConsumers: Int, containerCapacity: Int): Unit = {
    val buffer: mutable.Queue[Int] = new mutable.Queue[Int]()
    val producers = (1 to nProducers).map(id => new Producer(id, buffer, containerCapacity))
    val consumers = (1 to nConsumers).map(id => new Consumer(id, buffer, containerCapacity))
    for (elem <- producers) {elem.start()}
    for (elem <- consumers) {elem.start()}
  }
}