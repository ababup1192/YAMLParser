package org.ababup1192

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {

  "YAML parser" should "return an Empty-Map if it parses an empty brace" in {
    YAMLParser.parse( """{}""").get should be === Map()
  }

  it should "return an Empty-List if it parses an empty bracket" in {
    YAMLParser.parse( """[]""").get should be === List()
  }


  it should "return a simple map" in {
    YAMLParser.parse( """key: value""").get should be === Map("key" -> "value")
    YAMLParser.parse( """key1: value1
                         key2: value2 """).get should be === Map("key1" -> "value1", "key2" -> "value2")

    YAMLParser.parse( """key 1: value1
                         key 2: value2 """).get should be === Map("key 1" -> "value1", "key 2" -> "value2")
  }

  it should "return a simple list" in {
    YAMLParser.parse( """- item1""").get should be === List("item1")
    YAMLParser.parse(
      """- item1
         - item2""").get should be === List("item1", "item2")
  }

  /*
  it should "return 1ist of list" in {
    YAMLParser.parse(
      """-
           - item1
           - item2
         -
           - item3
           - item4""").get should be === List(List("item1", "item2"), List("item3", "item4"))
  }*/

  it should "return map of map" in {
    YAMLParser.parse(
      """human:
            name: John
            age: 15""").get should be === Map("human" -> Map("name" -> "John", "age" -> "15"))
  }

  it should "return a map if it parse inline map" in {
    YAMLParser.parse(
      """{key1: value1, key2: value2}""").get should be === Map("key1" -> "value1", "key2" -> "value2")
  }

  it should "return a list if it parse inline list" in {
    YAMLParser.parse(
      """[item1, item2]""").get should be === List("item1", "item2")
  }

  it should "return map with inline list" in {
    YAMLParser.parse(
      """foo: [item1, item2]""").get should be === Map("foo" -> List("item1", "item2"))
  }

  it should "return an list if it parses a list with map" in {
    YAMLParser.parse( """- foo
                         - {}
                         - bar""").get should be === List("foo", Map(), "bar")
  }

  it should "return an list if it parses a map with List and Map" in {
    YAMLParser.parse( """foo bar: true
                         hoge hoge: false
                         snafu: []
                         empty: {}"""
    ).get should be === Map("foo bar" -> true, "hoge hoge" -> false, "snafu" -> List(), "empty" -> Map())
  }

}
