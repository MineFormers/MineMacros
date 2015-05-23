package de.mineformers.mcreflect.introspection

import java.util.Objects
import scala.collection.mutable.{Map => MMap}

/**
 * RefConstructor
 *
 * @author PaleoCrafter
 */
class RefConstructor[C] private(val parent: RefType[C],
                                val descriptor: String,
                                val paramTypes: List[RefType[_]],
                                allowAccess: Boolean) {
  lazy val self = {
    val c = parent.runtimeClass.getDeclaredConstructor(paramTypes.map(_.runtimeClass): _*)
    if (allowAccess)
      c.setAccessible(true)
    c
  }

  def this(parent: RefType[C],
           descriptor: String,
           paramTypes: List[RefType[_]]) = this(parent, descriptor, paramTypes, false)

  def newInstance(params: Any*): Option[C] = try {
    Option(this(params: _*))
  } catch {
    case e: ReflectiveOperationException =>
      None
  }

  def apply(params: Any*): C = self.newInstance(params.map(_.asInstanceOf[Object]): _*)

  def unlocked = RefConstructor.accessible(parent, descriptor, paramTypes)

  override def toString: String = s"Constructor: $parent(${paramTypes.mkString(", ")})"

  override def hashCode(): Int = Objects.hash(parent.name, paramTypes)

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: RefConstructor[C] => c.parent == parent && c.descriptor == descriptor
    case _ => false
  }
}

object RefConstructor {
  private val cache = MMap.empty[String, RefConstructor[_]]
  private val accessibleCache = MMap.empty[String, RefConstructor[_]]

  def apply[C](parent: RefType[C], descriptor: String, paramTypes: List[RefType[_]]): RefConstructor[C] =
    cache.getOrElseUpdate(cacheName(parent, descriptor), new RefConstructor[C](parent, descriptor, paramTypes))
      .asInstanceOf[RefConstructor[C]]

  private def accessible[C](parent: RefType[C], descriptor: String, paramTypes: List[RefType[_]]): RefConstructor[C] =
    accessibleCache.getOrElseUpdate(cacheName(parent, descriptor), new RefConstructor[C](parent, descriptor,
      paramTypes, true))
      .asInstanceOf[RefConstructor[C]]

  private def cacheName(parent: RefType[_], descriptor: String) = parent.name + ":" + descriptor
}


