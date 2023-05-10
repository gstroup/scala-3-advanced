package com.rockthejvm.practice

import java.util.Date

object JSONSerialization {

  /***
   * creating a library to serialize objects to JSON: Users, posts, feeds
   * 1 - intermediate data types for JSON: numbers, strings, lists, objects
   * 2 - type class to convert data to intermediate data
   * 3 - serialize to JSON
   */
  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdDate: Date)
  case class Feed(user: User, posts: List[Post])

  // part 1
  sealed trait JSONValue {
    def stringify: String
  }

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = "\"" + value + "\""
  }

  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(values: List[JSONValue]) extends JSONValue {
    override def stringify: String = values.map(_.stringify).mkString("[", ",", "]")
  }

  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    override def stringify: String = values
      .map {
        case (key, value) => "\"" + key + "\":" + value.stringify
      }
      .mkString("{", ",", "}")
  }

  val data = JSONObject(Map(
    "user" -> JSONString("Greg"),
    "posts" -> JSONArray(List(
      JSONString("Scala study"),
      JSONNumber(82)
    ))
  ))

  // part 2 - type class pattern
  // 1 - TC def'n
  trait JSONConverter[T] {
    def convert(value: T): JSONValue
  }
  // 2 - TC instances for User, Post, Feed.  also String, Int, Date.
  given stringConverter: JSONConverter[String] with {
    override def convert(value: String): JSONValue = JSONString(value)
  }
  given intConverter :JSONConverter[Int] with {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }
  given dateConverter: JSONConverter[Date] with {
    override def convert(value: Date): JSONValue = JSONString(value.toString)
  }
  given userConverter: JSONConverter[User] with {
    override def convert(user: User): JSONValue = JSONObject(Map(
      "name" -> JSONConverter[String].convert(user.name),
      "age" -> JSONConverter[Int].convert(user.age),
      "email" -> JSONConverter[String].convert(user.email)
    ))
  }
  given postConverter: JSONConverter[Post] with {
    override def convert(post: Post): JSONValue = JSONObject(Map(
      "content" -> JSONConverter[String].convert(post.content),
      "createdDate" -> JSONConverter[String].convert(post.createdDate.toString)
    ))
  }
  given feedConverter: JSONConverter[Feed] with {
    override def convert(feed: Feed): JSONValue = JSONObject(Map(
      "user" -> JSONConverter[User].convert(feed.user),
      "posts" -> JSONArray(feed.posts.map(p => JSONConverter[Post].convert(p)))
    ))
  }

  // 3 - user-facing API
  object JSONConverter {
    def convert[T](value: T)(using converter: JSONConverter[T]): JSONValue =
      converter.convert(value)

    def apply[T](using instance: JSONConverter[T]): JSONConverter[T] = instance
  }
  // example
  val now = new Date(System.currentTimeMillis())
  val john = User("John", 34, "john@scala.com")
  val feed = Feed(john, List(
    Post("Hi, type classes are super fun", now),
    Post("Look at this puppy", now)
  ))

  // 4 - extension methods
  object JSONSyntax {
    extension [T](value: T)
      def toIntermediate(using converter: JSONConverter[T]): JSONValue =
        converter.convert(value)

      def toJSON(using converter: JSONConverter[T]): String =
        toIntermediate.stringify
  }

  def main(args: Array[String]): Unit = {
//    println(data.stringify)
    println(JSONConverter.convert(feed).stringify)

    import JSONSyntax.*
    println(feed.toIntermediate.stringify)
    println(feed.toJSON)
  }
}
