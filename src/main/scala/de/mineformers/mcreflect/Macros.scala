package de.mineformers.mcreflect

import scala.reflect.macros.blackbox
import de.mineformers.mcreflect.introspection.RefMethod
import de.mineformers.mcreflect.introspection.RefConstructor
import de.mineformers.mcreflect.introspection.RefType
import de.mineformers.mcreflect.introspection.RefField
import de.mineformers.mcreflect.mapping.Mappings
import scala.reflect.NameTransformer
import java.io.File
import scala.xml.TypeSymbol

/**
 * Macros
 *
 * @author PaleoCrafter
 */
object Macros {
  lazy val mappings = {
    val gradleStart = Class.forName("net.minecraftforge.gradle.GradleStartCommon")
    val field = gradleStart.getDeclaredField("SRG_DIR")
    field.setAccessible(true)
    val srgDir = field.get(null).asInstanceOf[File]
    Mappings.fromFile(new File(srgDir, "mcp-srg.srg").getAbsolutePath)
  }

  def typeImpl[C: c.WeakTypeTag](c: blackbox.Context): c.Expr[RefType[C]] = {
    import c.universe._
    val t = weakTypeTag[C]

    c.Expr[RefType[C]] {
      refType(c)(t.tpe)
    }
  }

  object constructors {

    def simpleConstructorImpl[C: c.WeakTypeTag](c: blackbox.Context): c.Expr[RefConstructor[C]] = {
      import c.universe._
      val classTag = c.weakTypeTag[C]
      val cType = classTag.tpe
      val className = cType.typeSymbol.fullName
      val alternatives = cType.members.filter(_.isConstructor)
      if (alternatives.isEmpty)
        c.abort(c.enclosingPosition, s"No constructors in $className")
      if (alternatives.size > 1)
        c.abort(c.enclosingPosition, s"There are multiple constructors in $className, " +
          s"try referencing it with tpe.constructor(type1, type2, ..., typeN)")
      val params = alternatives.head.asMethod.paramLists.head.map(_.info)

      val classLiteral = refType(c)(cType)
      val descriptor = buildDescriptor(c)(params, typeOf[Unit])
      val descLiteral = q"$descriptor"
      val types = params map (refType(c)(_))
      val typesLiteral = q"$types"

      c.Expr[RefConstructor[C]] {
        q""" de.mineformers.mcreflect.introspection.RefConstructor($classLiteral, $descLiteral, $typesLiteral) """
      }
    }

    def constructorImpl[C: c.WeakTypeTag](c: blackbox.Context)(args: c.Expr[RefType[_]]*): c.Expr[RefConstructor[C]] = {
      import c.universe._
      val classTag = c.weakTypeTag[C]
      val cType = classTag.tpe
      val className = cType.typeSymbol.fullName
      val alternatives = cType.members.filter(_.isConstructor)
      if (alternatives.isEmpty)
        c.abort(c.enclosingPosition, s"No constructors in $className")
      val types = args.toList map (_.actualType.typeArgs.head)
      val params = alternatives map (_.asMethod) find (m => m.paramLists.nonEmpty &&
        m.paramLists.head.length == types.length &&
        (m.paramLists.head, types)
        .zipped.forall(_.info =:= _)) match {
        case None =>
          c.abort(c.enclosingPosition, s"No constructor $className(${types.map(_.typeSymbol.fullName).mkString(", ")}):"
            + s" in $className")
        case Some(m) =>
          m.paramLists.head.map(_.info)
      }

      val classLiteral = refType(c)(cType)
      val descriptor = buildDescriptor(c)(params, typeOf[Unit])
      val descLiteral = q"$descriptor"
      val tpes = params map (refType(c)(_))
      val tpesLiteral = q"$tpes"

      c.Expr[RefConstructor[C]] {
        q""" de.mineformers.mcreflect.introspection.RefConstructor($classLiteral, $descLiteral, $tpesLiteral) """
      }
    }
  }

  object methods {

    def simpleMethodImpl[C: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(name: c.Expr[String]): c.Expr[RefMethod[C, R]] = {
      import c.universe._
      val classTag = c.weakTypeTag[C]
      val cType = classTag.tpe
      val returnTag = c.weakTypeTag[R]
      val rType = returnTag.tpe
      val className = cType.typeSymbol.fullName
      val retName = rType.typeSymbol.fullName
      val Literal(Constant(methodNameRaw: String)) = name.tree
      val methodName = NameTransformer.encode(methodNameRaw)
      val sym = cType.member(TermName(methodName))
      if (sym.alternatives.isEmpty)
        c.abort(c.enclosingPosition, s"Method $methodNameRaw is not defined in $className")
      if (sym.alternatives.count(_.asMethod.returnType =:= rType) > 1)
        c.abort(c.enclosingPosition, s"Method $className.$methodNameRaw: $retName  in  " +
          s"is ambiguous, try referencing it with tpe.methods.$methodNameRaw[$retName](type1, type2, ..., typeN)")
      val params = sym.alternatives map (_.asMethod) find (_.returnType =:= rType) match {
        case None =>
          c.abort(c.enclosingPosition, s"No method $methodNameRaw in $className with return type ${
            rType.typeSymbol.fullName
          }")
        case Some(m) =>
          if (m.paramLists.nonEmpty)
            m.paramLists.head.map(_.info)
          else
            Nil
      }

      val classLiteral = refType(c)(cType)
      val methLiteral = q"$methodName"
      val srgName = mappings.mapMethod(javaTypeName(c)(cType), methodName, javaTypeName(c)(rType), params map (javaTypeName(c)(_)))
      val srgLiteral = q"$srgName"
      val descriptor = buildDescriptor(c)(params, rType)
      val descLiteral = q"$descriptor"
      val returnTypeLiteral = refType(c)(rType)
      val tpes = params map (refType(c)(_))
      val typesLiteral = q"$tpes"

      c.Expr[RefMethod[C, R]] {
        q""" de.mineformers.mcreflect.introspection.RefMethod($classLiteral, $methLiteral, $srgLiteral, $descLiteral, $returnTypeLiteral, $typesLiteral) """
      }
    }

    def methodImpl[C: c.WeakTypeTag, R: c.WeakTypeTag](c: blackbox.Context)(name: c.Expr[String])(args: c.Expr[RefType[_]]*): c.Expr[RefMethod[C, R]] = {
      import c.universe._
      val classTag = c.weakTypeTag[C]
      val cType = classTag.tpe
      val returnTag = c.weakTypeTag[R]
      val rType = returnTag.tpe
      val className = cType.typeSymbol.fullName
      val retName = rType.typeSymbol.fullName
      val Literal(Constant(methodNameRaw: String)) = name.tree
      val methodName = NameTransformer.encode(methodNameRaw)
      val sym = cType.member(TermName(methodName))
      if (sym.alternatives.isEmpty)
        c.abort(c.enclosingPosition, s"Method $methodNameRaw is not defined in $className")
      val types = args.toList map (_.actualType.typeArgs.head)
      val params = sym.alternatives map (_.asMethod) find (m => m.returnType =:= rType &&
        m.paramLists.nonEmpty &&
        m.paramLists.head.length == types.length &&
        (m.paramLists.head, types)
        .zipped.forall(_.info =:= _)) match {
        case None =>
          c.abort(c.enclosingPosition, s"No method $methodNameRaw(${types.map(_.typeSymbol.fullName).mkString(", ")}): " +
            s"$retName in $className")
        case Some(m) =>
          m.paramLists.head.map(_.info)
      }

      val classLiteral = refType(c)(cType)
      val methLiteral = q"$methodName"
      val srgName = mappings.mapMethod(javaTypeName(c)(cType), methodName, javaTypeName(c)(rType), params map (javaTypeName(c)(_)))
      val srgLiteral = q"$srgName"
      val descriptor = buildDescriptor(c)(params, rType)
      val descLiteral = q"$descriptor"
      val returnTypeLiteral = refType(c)(rType)
      val tpes = params map (refType(c)(_))
      val typesLiteral = q"$tpes"

      c.Expr[RefMethod[C, R]] {
        q""" de.mineformers.mcreflect.introspection.RefMethod($classLiteral, $methLiteral, $srgLiteral, $descLiteral, $returnTypeLiteral, $typesLiteral) """
      }
    }
  }

  object fields {

    def fieldImpl[C: c.WeakTypeTag, T: c.WeakTypeTag](c: blackbox.Context)(name: c.Expr[String]): c.Expr[RefField[C, T]] = {
      import c.universe._
      val classTag = c.weakTypeTag[C]
      val cType = classTag.tpe
      val typeTag = c.weakTypeTag[T]
      val tType = typeTag.tpe
      val className = cType.typeSymbol.fullName
      val typeName = tType.typeSymbol.fullName
      val Literal(Constant(fieldNameRaw: String)) = name.tree
      val fieldName = NameTransformer.encode(fieldNameRaw)
      val sym = cType.member(TermName(fieldName))
      if (!sym.isTerm)
        c.abort(c.enclosingPosition, s"No such field '$fieldNameRaw' in $className")
      val term = sym.asTerm
      if ((term.isMethod && !term.isGetter) || (!term.isMethod && !(term.isVal ^ term.isVar)))
        c.abort(c.enclosingPosition, s"No such field '$fieldNameRaw' in $className")
      if ((term.isMethod && !(term.asMethod.returnType =:= tType)) || (!term.isMethod && !(term.info =:= tType)))
        c.abort(c.enclosingPosition, s"Field '$fieldNameRaw' in $className does not have type $typeName")

      val classLiteral = refType(c)(cType)
      val fieldLiteral = q"$fieldName"
      val srgName = mappings.mapField(javaTypeName(c)(cType), fieldName)
      val fieldSrgLiteral = q"$srgName"
      val byteType = nameFromType(c)(tType)
      val byteTypeLiteral = q"$byteType"
      val typeLiteral = refType(c)(tType)

      c.Expr[RefField[C, T]] {
        q""" de.mineformers.mcreflect.introspection.RefField[$cType, $tType]($classLiteral, $fieldLiteral, $fieldSrgLiteral, $byteTypeLiteral, $typeLiteral) """
      }
    }
  }

  private def refType(c: blackbox.Context)(t: c.Type) = {
    import c.universe._
    val name = javaTypeName(c)(t.dealias)
    q"""de.mineformers.mcreflect.introspection.RefType[$t]($name)"""
  }

  def javaTypeName(c: blackbox.Context)(t: c.Type): String = t.typeSymbol.fullName match {
    case "scala.Unit"    => "void"
    case "scala.Boolean" => "boolean"
    case "scala.Char"    => "char"
    case "scala.Byte"    => "byte"
    case "scala.Short"   => "short"
    case "scala.Int"     => "int"
    case "scala.Float"   => "float"
    case "scala.Long"    => "long"
    case "scala.Double"  => "double"
    case "scala.Array"   => javaTypeName(c)(t.typeArgs.head.dealias) + "[]"
    case x               => resolveInnerClass(c)(t)
  }

  private def buildDescriptor(c: blackbox.Context)(paramTypes: List[c.Type], returnType: c.Type): String = {
    paramTypes.map(x => nameFromType(c)(x)).mkString("(", "", ")") + nameFromType(c)(returnType)
  }

  def resolveInnerClass(c: blackbox.Context)(t: c.Type) = {
    import c.universe._

    def enclosingType(sym: Symbol): Symbol = {
      if (sym == NoSymbol) NoSymbol
      else if (sym.isType && (sym.owner.isPackage || sym.owner == NoSymbol)) sym
      else enclosingType(sym.owner)
    }

    def innerClassName(sym: Symbol) = {
      val tName = t.typeSymbol.fullName
      val sName = sym.fullName
      if (sName != tName) {
        val innerName = tName.replaceFirst(sName, "").replace(".", "$")
        sName + innerName
      } else
        sym.fullName
    }

    innerClassName(enclosingType(t.typeSymbol))
  }

  def nameFromType(c: blackbox.Context)(t: c.Type): String = {
    import c.universe._
    val vRef = typeOf[Unit]
    val zRef = typeOf[Boolean]
    val cRef = typeOf[Char]
    val bRef = typeOf[Byte]
    val sRef = typeOf[Short]
    val iRef = typeOf[Int]
    val fRef = typeOf[Float]
    val jRef = typeOf[Long]
    val dRef = typeOf[Double]
    c match {
      case _ if t =:= vRef => "V"
      case _ if t =:= zRef => "Z"
      case _ if t =:= cRef => "C"
      case _ if t =:= bRef => "B"
      case _ if t =:= sRef => "S"
      case _ if t =:= iRef => "I"
      case _ if t =:= fRef => "F"
      case _ if t =:= jRef => "J"
      case _ if t =:= dRef => "D"
    case _ if t.typeSymbol.fullName == "scala.Array"   => nameFromType(c)(t.typeArgs.head.dealias) + "[]"
      case _               => "L" + resolveInnerClass(c)(t).replace('.', '/') + ";"
    }
  }
}
