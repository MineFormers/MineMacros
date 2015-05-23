package de.mineformers.mcreflect.introspection

import scala.collection.mutable.{ Map => MMap }
import java.util.Objects
import scala.language.dynamics
import scala.language.experimental.macros
import de.mineformers.mcreflect.Macros
import java.lang.{ Void => JUnit, Boolean => JBoolean, Character => JChar, Byte => JByte, Short => JShort, Integer => JInt, Float => JFloat, Long => JLong, Double => JDouble }

/**
 * RefType
 *
 * @author PaleoCrafter
 */
class RefType[C](val name: String) {
  val methods = new Methods
  val fields = new Fields

  lazy val runtimeClass: Class[C] = (name match {
    case "vioid"   => JUnit.TYPE
    case "boolean" => JBoolean.TYPE
    case "char"    => JChar.TYPE
    case "byte"    => JByte.TYPE
    case "short"   => JShort.TYPE
    case "int"     => JInt.TYPE
    case "float"   => JFloat.TYPE
    case "long"    => JLong.TYPE
    case "double"  => JDouble.TYPE
    case _         => Class.forName(name)
  }).asInstanceOf[Class[C]]

  def newInstance = runtimeClass.newInstance()

  def constructor: RefConstructor[C] = macro Macros.constructors.simpleConstructorImpl[C]

  def constructor(args: RefType[_]*): RefConstructor[C] = macro Macros.constructors.constructorImpl[C]

  class Methods extends Dynamic {
    def applyDynamic[R](name: String)(args: RefType[_]*): RefMethod[C, R] = macro Macros.methods.methodImpl[C, R]

    def selectDynamic[R](name: String): RefMethod[C, R] = macro Macros.methods.simpleMethodImpl[C, R]
  }

  class Fields extends Dynamic {
    def selectDynamic[T](name: String): RefField[C, T] = macro Macros.fields.fieldImpl[C, T]
  }

  override def toString: String = name

  override def hashCode(): Int = Objects.hash(name)

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: RefType[C] => c.name == name
    case _             => false
  }
}

object RefType {
  private val cache = MMap.empty[String, RefType[_]]

  def of[C]: RefType[C] = macro Macros.typeImpl[C]

  def typeOf[C]: RefType[C] = macro Macros.typeImpl[C]

  def apply[C](name: String): RefType[C] = cache.getOrElseUpdate(name, new RefType[C](name)).asInstanceOf[RefType[C]]
}
