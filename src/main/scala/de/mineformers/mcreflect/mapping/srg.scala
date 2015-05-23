package de.mineformers.mcreflect.mapping

import scala.util.parsing.input.Reader
import java.io.FileReader

case class Mapping(srg: String, mcp: String)

case class SrgFile(entries: Seq[SrgFileEntry])

sealed trait SrgFileEntry {
  def className: String
}

case class SrgFileClass(className: String) extends SrgFileEntry

case class SrgFileField(className: String, fieldName: Mapping) extends SrgFileEntry

case class SrgFileMethod(className: String, methodName: Mapping, returnType: String, paramTypes: String*) extends SrgFileEntry

sealed trait SrgEntry

class Mappings(val classes: Map[String, SrgClass]) {
  def mapMethod(cls: String, mcpName: String, returnType: String, paramTypes: Seq[String]): String =
    classes.get(cls) map (_.methods.find(m => m.name.mcp == mcpName && m.returnType == returnType
      && m.paramTypes == paramTypes) map (_.name.srg) getOrElse mcpName) getOrElse mcpName

  def mapField(cls: String, mcpName: String): String =
    classes.get(cls) map (_.fields.find(_.name.mcp == mcpName) map
      (_.name.srg) getOrElse mcpName) getOrElse mcpName
}

object Mappings {
  def apply(file: SrgFile): Mappings = {
    val rawClasses = file.entries.filter(_.isInstanceOf[SrgFileClass]).map(_.asInstanceOf[SrgFileClass])
    val rawFields = file.entries.filter(_.isInstanceOf[SrgFileField]).map(_.asInstanceOf[SrgFileField])
    val rawMethods = file.entries.filter(_.isInstanceOf[SrgFileMethod]).map(_.asInstanceOf[SrgFileMethod])

    val classes = rawClasses map {
      c =>
        val fields = rawFields.filter(_.className == c.className).map(f => SrgField(f.fieldName))
        val methods = rawMethods.filter(_.className == c.className).map(m => SrgMethod(m.methodName, m.returnType, m.paramTypes))
        (c.className, SrgClass(c.className, fields, methods))
    }

    new Mappings(classes.toMap)
  }

  def fromFile(path: String): Mappings = {
    val parseResult = new SrgParser().parse(new FileReader(path))
    parseResult map apply getOrElse null
  }
}

case class SrgClass(name: String, fields: Seq[SrgField], methods: Seq[SrgMethod]) extends SrgEntry

case class SrgField(name: Mapping) extends SrgEntry

case class SrgMethod(name: Mapping, returnType: String, paramTypes: Seq[String]) extends SrgEntry