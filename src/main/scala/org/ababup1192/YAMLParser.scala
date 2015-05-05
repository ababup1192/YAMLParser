package org.ababup1192

import scala.util.parsing.combinator._

object YAMLParser extends RegexParsers {
  override def skipWhitespace = false

  val mapSeparator = ": *\r?\n?".r

  val seqIndicator = "- *\r?\n?" r

  val mappingKey = """[^-:\r\n\}\{\[]+""".r

  val newLine = " *\r*\n+".r

  val inlineSeparator = " *, +".r

  val openBrace = """\{ *""".r

  val closeBrace = """ *\}""".r

  val openBracket = """\[ *""".r

  val closeBracket = """ *]""".r

  def leadingSpaces(numLeadingSpaces: Int): Parser[Int] = ("^ {" + numLeadingSpaces + ",}" r) ^^ {
    _.length
  }

  def mappings(numLeadingSpaces: Int): Parser[Map[String, Any]] =
    (indentedMap(numLeadingSpaces) | inlineMap) ^^ {
      Map() ++ _
    }

  def indentedMap(numLeadingSpaces: Int): Parser[List[(String, Any)]] = rep1sep(indentedMapping(numLeadingSpaces), newLine)

  def inlineMap: Parser[List[(String, Any)]] = openBrace ~> repsep(inlineMapping, inlineSeparator) <~ closeBrace

  def indentedMapping(numLeadingSpaces: Int): Parser[(String, Any)] =
    leadingSpaces(numLeadingSpaces) ~> mappingKey ~ mapSeparator ~
      (list(0) | mappings(numLeadingSpaces + 1) | scalarData( """[^\r\n]+""")) ^^ {
      case key ~ _ ~ value => (key.trim, value)
    }

  def inlineMapping: Parser[(String, Any)] =
    mappingKey ~ mapSeparator ~ (inlineList | inlineMap | scalarData( """[^,\r\n\}]+""")) ^^ {
      case key ~ _ ~ value => (key.trim, value)
    }

  def list(numLeadingSpaces: Int): Parser[List[Any]] =
    (inlineList | indentedList(numLeadingSpaces)) ^^ {
      List() ++ _
    }

  def indentedList(numLeadingSpaces: Int): Parser[List[Any]] = {
    rep1sep(leadingSpaces(numLeadingSpaces) ~ seqIndicator ~> nestedListData(numLeadingSpaces), newLine)
  }

  def inlineList: Parser[List[Any]] =
    openBracket ~> repsep(nestedListData(0), inlineSeparator) <~ closeBracket

  def nestedListData(numLeadingSpaces: Int): Parser[Any] = list(numLeadingSpaces + 1) |
    mappings(numLeadingSpaces) | scalarData( """[^,\r\n\]]+""")

  // TODO: Implement scalar data
  def scalarData(regexString: String): Parser[Any] = regexString.r ^^ { value =>
    value.trim match {
      case "true" => true
      case "false" => false
      case v => v
    }
  }

  def yaml: Parser[Any] = opt(newLine) ~> (list(0) | mappings(0))

  def parse(text: String): ParseResult[Any] = {
    parse(yaml, text)
  }

}
