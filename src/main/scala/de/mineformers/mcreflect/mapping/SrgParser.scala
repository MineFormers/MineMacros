package de.mineformers.mcreflect.mapping

import scala.util.parsing.combinator._
import java.io.Reader

class SrgParser extends JavaTokenParsers {
  private val className: Parser[String] = """([a-zA-Z0-9_]*\/)*[a-zA-Z0-9_$]+""".r
  private val classPrefix: Parser[String] = """([a-zA-Z0-9_$]*\/)+""".r
  private val typeByteCode: Parser[String] = """V|Z|C|B|S|I|F|J|D""".r
  private val classByteCode: Parser[String] = """L([a-zA-Z0-9_]*\/)*[a-zA-Z0-9$_]+;""".r

  def file: Parser[SrgFile] = rep(entry) ^^ SrgFile

  def entry: Parser[SrgFileEntry] = classEntry | fieldEntry | methodEntry

  def classEntry: Parser[SrgFileClass] = "CL: " ~ className ~ className ^^ {
    case _ ~ cl ~ _ => SrgFileClass(cl.replace("/", "."))
  }

  def fieldEntry: Parser[SrgFileField] = "FD: " ~ classPrefix ~ ident ~ classPrefix ~ ident ^^ {
    case _ ~ cl ~ mcp ~ _ ~ srg => SrgFileField(cl.replace("/", ".").init, Mapping(srg, mcp))
  }

  def methodEntry: Parser[SrgFileMethod] = "MD: " ~ classPrefix ~ ident ~ methodDescriptor ~ classPrefix ~ ident ~ methodDescriptor ^^ {
    case _ ~ cl ~ mcp ~ desc ~ _ ~ srg ~ _ => SrgFileMethod(cl.replace("/", ".").init, Mapping(srg, mcp), desc._1, desc._2: _*)
  }

  def methodDescriptor: Parser[(String, Seq[String])] = "(" ~ rep(methodType) ~ ")" ~ methodType ^^ {
    case _ ~ paramTypes ~ _ ~ returnType => (returnType, paramTypes)
  }

  def methodType: Parser[String] = opt(rep("[")) ~ (classByteCode | typeByteCode) ^^ {
    case a ~ c if c.length > 1 => c.init.tail.replace("/", ".") + (if (a.isDefined) "[]" * a.get.length else "")
    case a ~ t                 => javaTypeName(t) + (if (a.isDefined) "[]" * a.get.length else "")
  }

  def javaTypeName(t: String): String = t match {
    case "V" => "void"
    case "Z" => "boolean"
    case "C" => "char"
    case "B" => "byte"
    case "S" => "short"
    case "I" => "int"
    case "F" => "float"
    case "J" => "long"
    case "D" => "double"
    case x   => x
  }

  def parse(input: Reader) = parseAll(file, input)

  def parse(input: String) = parseAll(file, input)
}