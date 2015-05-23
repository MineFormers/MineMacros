package de.mineformers.mcreflect.introspection

import java.util.Objects
import scala.collection.mutable.{ Map => MMap }
import de.mineformers.mcreflect.mapping.MapUtil

/**
 * RefMethod
 *
 * @author PaleoCrafter
 */
class RefMethod[C, R] private (val parent: RefType[C],
                               val mcpName: String,
                               val srgName: String,
                               val descriptor: String,
                               val returnType: RefType[R],
                               val paramTypes: List[RefType[_]],
                               allowAccess: Boolean) {
  lazy val self = {
    val m = parent.runtimeClass.getDeclaredMethod(name, paramTypes.map(_.runtimeClass): _*)
    if (allowAccess)
      m.setAccessible(true)
    m
  }

  def this(parent: RefType[C],
           mcpName: String,
           srgName: String,
           descriptor: String,
           returnType: RefType[R],
           paramTypes: List[RefType[_]]) = this(parent, mcpName, srgName, descriptor, returnType, paramTypes, false)

  def name = if (MapUtil.srgRequired) srgName else mcpName

  def invoke(target: C, params: Any*): Option[R] = try {
    Option(this(target)(params: _*))
  } catch {
    case e: ReflectiveOperationException =>
      None
  }

  def apply(target: C)(params: Any*): R = self.invoke(target, params.map(_.asInstanceOf[Object]): _*).asInstanceOf[R]

  def unlocked = RefMethod.accessible(parent, mcpName, srgName, descriptor, returnType, paramTypes)

  override def toString: String = s"Method: $parent#name(${paramTypes.mkString(", ")}): $returnType"

  override def hashCode(): Int = Objects.hash(parent.name, mcpName, srgName, descriptor)

  override def equals(obj: scala.Any): Boolean = obj match {
    case c: RefMethod[C, R] => c.parent == parent && c.mcpName == mcpName && c.srgName == srgName && c.descriptor == descriptor
    case _                  => false
  }
}

object RefMethod {
  private val cache = MMap.empty[String, RefMethod[_, _]]
  private val accessibleCache = MMap.empty[String, RefMethod[_, _]]

  def apply[C, R](parent: RefType[C],
                  mcpName: String,
                  srgName: String,
                  descriptor: String,
                  returnType: RefType[R],
                  paramTypes: List[RefType[_]]): RefMethod[C, R] =
    cache.getOrElseUpdate(cacheName(parent, mcpName, descriptor), new RefMethod[C, R](parent, mcpName, srgName, descriptor,
      returnType, paramTypes))
      .asInstanceOf[RefMethod[C, R]]

  private def accessible[C, R](parent: RefType[C],
                               mcpName: String,
                               srgName: String,
                               descriptor: String,
                               returnType: RefType[R],
                               paramTypes: List[RefType[_]]): RefMethod[C, R] =
    accessibleCache.getOrElseUpdate(cacheName(parent, mcpName, descriptor), new RefMethod[C, R](parent, mcpName, srgName,
      descriptor, returnType, paramTypes, true))
      .asInstanceOf[RefMethod[C, R]]

  private def cacheName(parent: RefType[_],
                        name: String,
                        descriptor: String) = parent.name + ":" + name + ":" + descriptor
}
