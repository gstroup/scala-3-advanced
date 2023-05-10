package com.rockthejvm.part3async

import java.util.concurrent.Executors
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration.*
import scala.util.{Failure, Random, Success, Try}

object Futures {

  def calculateMeaningOfLife(): Int = {
    // simulate long compute
    Thread.sleep(1000)
    42
  }

  // java thread pool
  val executor = Executors.newFixedThreadPool(4)
  // this is a scala native thread pool.  wrapping a java thread pool.
  given executionContext: ExecutionContext = ExecutionContext.fromExecutorService(executor)
  // declare as a "given" so it can be passed implicitly to onComplete()

  // a future = an async computation that will finish at some point in the future.  (runs on another thread)
  val aFuture: Future[Int] = Future(calculateMeaningOfLife()) //(executionContext)

  // inspect the value of the future right NOW.  may have a value, may not. or it might have an exception.
  val futureInstantResult: Option[Try[Int]] = aFuture.value

  // instead of waiting while(true), use a callback:
  aFuture.onComplete {
    // remember: Success & Failure are subclasses of Try
    case Success(value) => println(s"Completed with value: $value")
    case Failure(e) => println(s"failed with exception: $e")
  }  // this could be executed on some other thread.

  /**
   * Functional composition
   */
  case class Profile(id: String, name: String) {
    def sendMessage(anotherProfile: Profile, message: String) =
      println(s"${this.name} sending message to ${anotherProfile.name}: $message")
  }

  object SocialNetwork {
    // "database" of users
    val names = Map(
      "rtjvm.id.1-daniel" -> "Daniel",
      "rtjvm.id.1-greg" -> "Greg",
      "rtjvm.id.1-con" -> "Con",
    )

    val friends = Map(
      "rtjvm.id.1-greg" -> "rtjvm.id.1-con"
    )

    val random = new Random()

    // "API" - asynchronous
    def fetchProfile(id: String): Future[Profile] = Future {
      // fetch from db
      Thread.sleep(random.nextInt(300)) // simulate db call
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(360))
      val bestFriendId = friends(profile.id)
      Profile(bestFriendId, names(bestFriendId))
    }
  }

  // problem: send a message to my BFF
  def sendMessageToBestFriend(accountId: String, message: String): Unit = {
    val profileFuture = SocialNetwork.fetchProfile(accountId)
    profileFuture.onComplete {
      case Success(profile) => {
        val friendProfileFuture = SocialNetwork.fetchBestFriend(profile)
        friendProfileFuture.onComplete {  // getting ugly.  "callback hell"
          case Success(friendProfile) => profile.sendMessage(friendProfile, message)
          case Failure(e) => e.printStackTrace()
        }
      }
      case Failure(e) => e.printStackTrace()
    }
  }
  // onComplete is a hassle!
  // solution is Functional composition.  (map, flatMap, filter)
  // this is a little better, but not much
  def sendMessageToBestFriend2(accountId: String, message: String): Unit = {
    val profileFuture = SocialNetwork.fetchProfile(accountId)
    profileFuture.flatMap { profile =>  // Future[Unit]
      SocialNetwork.fetchBestFriend(profile).map { bff => // Future[Unit]
        profile.sendMessage(bff, message) // returns Unit
      }
    }
  }
  // use for-comprehension.  much better!
  def sendMessageToBestFriend3(accountId: String, message: String): Unit = {
    for {
      profile <- SocialNetwork.fetchProfile(accountId)
      bff <- SocialNetwork.fetchBestFriend(profile)
    } yield profile.sendMessage(bff, message)  // for the compiler, this is identical to v2.
  }

  val conProfileFuture = SocialNetwork.fetchProfile("rtjvm.id.1-con")
  val conFuture: Future[String] = conProfileFuture.map(_.name)  // map transforms value contained inside asynchronously
  val consBestFriend: Future[Profile] = conProfileFuture.flatMap(profile => SocialNetwork.fetchBestFriend(profile))
  val consBestFriendFilter: Future[Profile] = consBestFriend.filter(profile => profile.name.startsWith("Z"))

  // fallbacks - handle errors from Futures
  val profileNoMatterWhat: Future[Profile] = SocialNetwork.fetchProfile("unknown").recover {
    case e: Throwable => Profile("rtjvm.id.0-dummy", "Dummy account")
  }

  // analogy:  recoverWith is to recover what flatMap is to map
  val aFetchedProfileNoMatterWhat: Future[Profile] = SocialNetwork.fetchProfile("unknown").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("rtjvm.id.1-dummy")
  } // if both futures fail, exception returned is from the second

  val fallbackProfile: Future[Profile] = SocialNetwork.fetchProfile("unknown")
    .fallbackTo(SocialNetwork.fetchProfile("rtjvm.id.1-dummy"))
    // if both futures fail, exception returned is from the first

  /**
   * Block for a future.  Not recommended unless it's really necessary.
   */
  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)
  object BankingApp {
    def fetchUser(name: String): Future[User] = Future {
      // simulate db fetch
      Thread.sleep(400)
      User(name)
    }

    def createTransaction(user: User, merchant: String, amount: Double): Future[Transaction] = Future {
      Thread.sleep(1000)
      Transaction(user.name, merchant, amount, "SUCCESS")
    }

    // "external" fake API - pretend we're forced to make this async call synchronous.
    def purchase(username: String, item: String, merchant: String, price: Double): String = {
      // here we're returning a String, not a Future
      val transactionFuture: Future[String] = for {
        user <- fetchUser(username)
        tx <- createTransaction(user, merchant, price)
      } yield tx.status

      // blocking call - try not to do this!  use Futures when you can.
      Await.result(transactionFuture, 2.seconds) // "seconds" method is from scala.util.Duration
        // throws TimeoutException if future doesn't finish within 2 seconds
    }
  }

  /**
   * Promises
   * a promise is a wrapper around a future
   */
  def demoPromises(): Unit = {
    val promise = Promise[Int]()
    val futureInside: Future[Int] = promise.future

    // thread 1 (main thread) - consumer: monitor the future for completion
    futureInside.onComplete {
      case Success(value) => println(s"[consumer] I have completed with $value")
      case Failure(e) => e.printStackTrace()
    }

    // thread 2 - producer
    val producerThread = new Thread(() => {
      println("[producer] crunching numbers...")
      Thread.sleep(1000)
      // fulfill the promise
      promise.success(42)
      println("[producer] - done.")
    })

    producerThread.start()
  }

  /*
  EXERCISES
  1 - fulfill a future immediately with a value
  2 - define a method that takes 2 futures "inSequence"
  3 - create a method: first(fa, fb) => new Future with the value of the First future to complete
  4 - last(fa, fb) => new Future with the value of the Last future to complete
  5 - retry an action returning a future until a predicate holds true
   */

  // 1
  // my naive guess:
//  def fulfiller(): Unit = {
//    val aPromise = Promise[Int]()
//    aPromise.success(33)
//  }

  // his answer
  def completeImmediately[A](value: A): Future[A] = Future(value) // async completion ASAP (any thread)
  // extra solution:
  def completeImmediately2[A](value: A): Future[A] = Future.successful(value)  // synchronous completion (same thread)

  // 2
  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = {
    // my solution: (I think this works too, but not as concise)
//    first.onComplete {
//      case Success(_) => second
//      case Failure(e) => e.printStackTrace()
//    }
    // his soln:
    first.flatMap(_ => second)
  }

  // 3
  def first[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val promise = Promise[A]()
    f1.onComplete(result1 => promise.tryComplete(result1))  // these 2 futures will compete for completion of the promise
    f2.onComplete(result2 => promise.tryComplete(result2))
    // promise.complete() can only be called once!  so we use tryComplete() instead.
    // calling complete on an already-completed promise will throw an error.
    promise.future
  }

  // 4
  def last[A](f1: Future[A], f2: Future[A]): Future[A] = {
    val sharedPromise = Promise[A]()
    val lastPromise = Promise[A]()

    def checkAndComplete(result: Try[A]): Unit = {
      if (!sharedPromise.tryComplete(result))
        lastPromise.complete(result)
    }
    f1.onComplete(checkAndComplete)
    f2.onComplete(checkAndComplete)
    lastPromise.future
  }

  def testFirstAndLast(): Unit = {
    lazy val fast = Future {
      Thread.sleep(100)
      1
    }
    lazy val slow = Future {
      Thread.sleep(200)
      2
    }
    first(fast, slow).foreach(result => println(s"FIRST: $result"))
    last(fast, slow).foreach(result => println(s"LAST: $result"))
  }

  // 5 - invoke action that returns a future.  if predicate is true, return.  if not retry
  def retryUntil[A](action: () => Future[A], predicate: A => Boolean): Future[A] = {
    action()
      .filter(predicate)
      .recoverWith {
        case _ => retryUntil(action, predicate)
      }
  }

  def testRetries(): Unit = {
    val random = new Random()
    val action = () => Future {
      Thread.sleep(100)
      val nextValue = random.nextInt(100)
      println(s"generated $nextValue")
      nextValue
    }

    val predicate = (x: Int) => x < 10
    retryUntil(action, predicate).foreach(finalResult => println(s" final result: $finalResult"))
  }

  def main(args: Array[String]): Unit = {
//    println(futureInstantResult)
//    sendMessageToBestFriend3("rtjvm.id.1-greg", "What's up?")

//    println("purchasing...")
//    println(BankingApp.purchase("greg-234", "tall can", "lunardis-823", 5.99))
//    println("purchase complete")

//    demoPromises()

//    testFirstAndLast()
    testRetries()

    Thread.sleep(2000)
    executor.shutdown()
  }
}
