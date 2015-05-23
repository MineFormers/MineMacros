package de.mineformers.mcreflect.introspection

import java.util.Objects
import scala.collection.mutable.{ Map => MMap }
import java.lang.reflect.Modifier
import de.mineformers.mcreflect.mapping.MapUtil

/**
 * RefField
 *
 * @author PaleoCrafter
 */
class RefField[C, T] private (val parent: RefType[C],
                              val mcpName: String,
                              val srgName: String,
                              val byteType: String,
                              val tpe: RefType[T],
                              allowAccess: Boolean) {
  lazy val self = {
    val f = parent.runtimeClass.getDeclaredField(name)
    if (allowAccess)
      f.setAccessible(true)
    f
  }

  def this(parent: RefType[C],
           mcpName: String,
           srgName: String,
           byteType: String,
           tpe: RefType[T]) = this(parent, mcpName, srgName, byteType, tpe, false)

  def name = if (MapUtil.srgRequired) srgName else mcpName

  def get[C1 <: C](holder: C1): Option[T] = try {
    Option(value(holder))
  } catch {
    case e: ReflectiveOperationException =>
      None
  }

  def mutable = !Modifier.isFinal(self.getModifiers)

  def value[C1 <: C](holder: C1): T = self.get(holder).asInstanceOf[T]

  def update[C1 <: C](holder: C1, value: T) = self.set(holder, value)

  def unlocked = RefField.accessible(parent, mcpName, srgName, byteType, tpe)

  override def toString: String = s"Field: $parent#$name: $tpe"

  override def hashCode(): Int = Objects.hash(parent.name, mcpName, srgName, byteType)

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: RefField[C, T] => c.parent == parent && c.mcpName == mcpName && c.srgName == srgName && c.byteType == byteType
    case _                 => false
  }
}

object RefField {
  private val cache = MMap.empty[String, RefField[_, _]]
  private val accessibleCache = MMap.empty[String, RefField[_, _]]

  def apply[C, T](parent: RefType[C],
                  mcpName: String,
                  srgName: String,
                  byteType: String,
                  tpe: RefType[T]): RefField[C, T] =
    cache.getOrElseUpdate(cacheName(parent, mcpName, byteType), new RefField(parent, mcpName, srgName, byteType, tpe))
      .asInstanceOf[RefField[C, T]]

  private def accessible[C, T](parent: RefType[C],
                               mcpName: String,
                               srgName: String,
                               byteType: String,
                               tpe: RefType[T]): RefField[C, T] =
    accessibleCache.getOrElseUpdate(cacheName(parent, mcpName, byteType), new RefField(parent, mcpName, srgName, byteType,
      tpe, true))
      .asInstanceOf[RefField[C, T]]

  private def cacheName(parent: RefType[_],
                        name: String,
                        descriptor: String) = parent.name + ":" + name + ":" + descriptor
}

