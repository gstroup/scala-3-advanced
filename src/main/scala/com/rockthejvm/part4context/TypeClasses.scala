package com.rockthejvm.part4context

object TypeClasses {

  /**
   * example: build a small library to serialize some data to a standard format (HTML)
   */

  // V1: OO way is to create an interface / trait, and some serializers
  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age) <a href=$email/></div>"
  }
  val bob = User("Bob", 33, "bob@scala.com")
  val bob2Html = bob.toHtml
  // same pattern for other data structures that we want to serialize
  // DRAWBACKS:
  //  - only available for the types WE write
  //  - can only have one implementation

  // V2: Scala way - pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any): String = value match {
      case User(name, age, email) => s"<div>$name ($age) <a href=$email/></div>"
      case _ => throw new IllegalArgumentException("data structure not supported yet")
    }
  }
  // DRAWBACKS:
  //  - lost type safety
  //  - need to modify a single piece of code every time
  //  - still ONE implementation

  // ************************
  // V3 - type class!
  // part 1 - type class definition
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }
  // part 2 - type class instances for the supported types
  given userSerializer: HTMLSerializer[User] with {
    override def serialize(value: User): String = {
      val User(name, age, email) = value
      s"<div>$name ($age) <a href=$email/></div>"
    }
  }
  val bob2Html2 = userSerializer.serialize(bob)
  // BENEFITS:
  // - can define serializers for other types outside the library
  // - multiple serializers for the same type are supported. developer can pick one.
  import java.util.Date
  given dateSerialize: HTMLSerializer[Date] with {
    override def serialize(date: Date): String = s"<div>${date.toString()}</div>"
  }
  object SomeOtherSerializers {
    given partialUserSerializer: HTMLSerializer[User] with {
      override def serialize(user: User): String = s"<div>$user.name</div>"
    }
  }
  // part 3 - exposing the type class in a user-facing API
  object HTMLSerializer {
    def serialize[T](value: T)(using serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    // this makes it more obvious which type class we're using:
    def apply[T](using serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer
  }
  val bob2Html3 = HTMLSerializer.serialize(bob)
  val bob2Html4 = HTMLSerializer[User].serialize(bob)
  // part 4 - simplify the method call using an extension method
  object HTMLSyntax {
    extension [T](value: T)
      // "using" here means the method requires a given of HTMLSerializer[T]
      def toHTML(using serializer: HTMLSerializer[T]): String = serializer.serialize(value)
  }

  import HTMLSyntax.*
  val bob2Html5 = bob.toHTML  // works since we've defined a generic extension, and we have the right serializer in scope.
  // now we have the "best of both worlds":
  // - simple way to call the method ("expressiveness"),
  // - type safety
  // - ability to add more implementations in different places.
  // - ability to extend functionality to new types
  // - ability to choose impl, buy importing the right given.

  def main(args: Array[String]): Unit = {
    println(bob2Html)
    println(bob2Html2)
    println(bob2Html3)
    println(bob2Html5)
  }
}
