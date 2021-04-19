package pbconverts

import com.google.protobuf.MessageLite.Builder
import com.google.protobuf.{ByteString, GeneratedMessageV3, Message}
import quoted.{Expr, Quotes, Type}

import scala.collection.mutable

// scalastyle:off number.of.methods
class ProtoScalableMacro[T,M<:GeneratedMessageV3](using typeT:Type[T], typeM:Type[M])(using quotas:Quotes) {
  import quotas.reflect._

  private[this] val scalaClassType = quotas.reflect.TypeRepr.of[T](using typeT)
  private[this] val protoClassType = quotas.reflect.TypeRepr.of[M](using typeM)

  private[this] val protoClassSymbol: quotas.reflect.Symbol = quotas.reflect.TypeReprMethods.classSymbol(protoClassType).get
  private[this] val protoClassSymbolCompanion = quotas.reflect.SymbolMethods.companionClass(protoClassSymbol)
  private[this] val builderSymbol = quotas.reflect.SymbolMethods.declaredMethod(protoClassSymbolCompanion)("newBuilder").head
//  private[this] val newBuilderTree = Apply(builderSymbol.tree.asInstanceOf[Term], List.empty)
  private[this] val newProtoTree =  New(protoClassSymbol.tree.asInstanceOf[TypeTree]).asExprOf[M]






  private val x = quotas.reflect.SymbolMethods.memberMethod(protoClassSymbolCompanion)("newBuilder").head
  println(x)

  Apply(protoClassSymbol.tree)


  private[this] def packageName  = ???//q"_root_.pbconverts"
  private[this] def builderIdent = ???//Ident(TermRef("builder"))
  private[this] def entityIdent = ??? //Ident.apply(TermRef(,"entity"))  //Ident(TermRef("entity"))
  private[this] def protoIdent = ??? //Ident(TermRef("proto"))
  private[this] def implicitlyProtoable(entityType: TypeRepr, protoType: TypeRepr) = ??? //q"implicitly[$packageName.Protoable[$entityType,$protoType]]"

  private[this] def implicitlyScalable(entityType: TypeRepr, protoType: TypeRepr) = ??? //q"implicitly[$packageName.Scalable[$entityType,$protoType]]"

  private[this] def isOption(tpe: TypeRepr): Boolean = {
    tpe <:< TypeRepr.of[Option[_]]
  }

  private[this] def isIterable(tpe: TypeRepr): Boolean =
    (tpe <:< TypeRepr.of[Iterable[_]]
      || tpe <:< TypeRepr.of[Array[_]]
      || tpe <:< TypeRepr.of[java.lang.Iterable[_]]) && !(tpe <:< TypeRepr.of[Map[_, _]] || tpe <:< TypeRepr.of[ByteString])

  private[this] def isMap(tpe: TypeRepr): Boolean = tpe <:< TypeRepr.of[Map[_, _]] || tpe <:< TypeRepr.of[java.util.Map[_, _]]

  private[this] def getBuilderType(): TypeRepr = {
//    TypeReprMethods
//    protoType.companion.member(TermName("newBuilder")).asTerm.alternatives.head.asMethod.returnType
    ???
  }

  private trait Processor {

    def scalaTreeType: TypeRepr
    def protoValueType: TypeRepr

    def resolvedCaseClassFieldType: TypeRepr = resolveFieldType(scalaTreeType)

    def tree: Option[Tree]
  }

  // Convert expr (`scalaTree` in this class) to protobuf value
  private class ToProtoProcessor(
      scalaTree: Tree,
      override val scalaTreeType: TypeRepr,
      override val protoFieldName: String
  ) extends ProtoFieldAnalyst {

    private[this] def toProto(t1: TypeRepr, t2: TypeRepr): Tree = {
      if (t1 <:< t2) {
        scalaTree
      } else {
        ???
//        q"${implicitlyProtoable(t1, t2)}.toProto($scalaTree)"
      }
    }

    override def protoValueType: TypeRepr = protoValueSetterType

    def optTree(optFieldType: TypeRepr): Tree = {
      val tree = if (isIterable(protoValueType)) {
        seqTree(optFieldType)
      } else if (isMap(protoValueType)) {
        mapTree(optFieldType)
      } else {
        defaultTree(optFieldType)
      }
      ???
//      If(q"$scalaTree.isDefined", tree, EmptyTree)
    }

    def seqTree(iterableType: TypeRepr): Tree = {
//      val listType = builderType.member(addAllMethodName).asMethod.paramLists.head.head.typeSignature
//      val valueTree = toProto(iterableType, listType)
//      q"${builderIdent}.$addAllMethodName($valueTree)"
      ???
    }

    def mapTree(mapType: TypeRepr): Tree = {
//      val putAllMapType = builderType.member(putAllMethodName).asMethod.paramLists.head.head.typeSignature
//      val valueTree = toProto(mapType, putAllMapType)
//      q"${builderIdent}.$putAllMethodName($valueTree)"
      ???
    }

    def defaultTree(fieldType: TypeRepr): Tree = {
//      val valueTree = toProto(fieldType, protoValueType)
//      q"${builderIdent}.$setField(${valueTree})"
      ???
    }

    override def tree: Option[Tree] = {
//      if (protoValueType == NoType) {
      if (protoValueType == ???) {
          None
      } else {
        val _tree = if (isOption(scalaTreeType)) {
          optTree(resolvedCaseClassFieldType)
        } else if (isIterable(protoValueType)) {
          seqTree(resolvedCaseClassFieldType)
        } else if (isMap(protoValueType)) {
          mapTree(resolvedCaseClassFieldType)
        } else {
          defaultTree(resolvedCaseClassFieldType)
        }

        if (scalaTreeType <:< TypeRepr.of[AnyRef]) {
//          val notNullCondition = q"$scalaTree ne null"
//          Some(If(notNullCondition, _tree, EmptyTree))
          ???
        } else {
          Some(_tree)
        }
      }
    }
  }

  // Convert expr (`protoValueTree` in this class) to case class filed value
  private case class ToScalaProcessor(caseClassSelector: Symbol, protoValueTree: Tree, protoValueType: TypeRepr)
      extends AbstractToScalaProcessor

  private trait AbstractToScalaProcessor extends Processor {

    def caseClassSelector: Symbol

    def protoValueTree: Tree

    override def scalaTreeType: TypeRepr = ???//caseClassSelector.returnType

    protected def toScala(t1: TypeRepr, t2: TypeRepr, value: Tree) = {
      if (t2 <:< t1) {
        value
      } else {
//        q"${implicitlyScalable(t1, t2)}.toScala($value)"
        ???
      }
    }

    def optTree(optType: TypeRepr): Tree = defaultTree(optType)

    def defaultTree(fieldType: TypeRepr): Tree = {
//      q"$caseClassSelector = ${toScala(fieldType, protoValueType, protoValueTree)}"
      ???
    }

    override def tree: Option[Tree] =
      if (protoValueType == ???) {
//        if (protoValueType == NoType) {
        None
      } else {
        if (isOption(scalaTreeType)) {
          Some(optTree(resolvedCaseClassFieldType))
        } else {
          Some(defaultTree(resolvedCaseClassFieldType))
        }
      }
  }

  // Contain some methods and vals to analyze Protobuf fields
  private trait ProtoFieldAnalyst extends Processor {
    def protoFieldName: String

    val builderType = getBuilderType()


    def addAllMethodName: ByName = ???//ByName(s"addAll$protoFieldName")
    def putAllMethodName: ByName = ???//ByName(s"putAll$protoFieldName")

    def addField: ByName = ???//TermName(s"add$protoFieldName")
    def putField: ByName = ???//TermName(s"put$protoFieldName")
    def setField: ByName = ???//TermName(s"set$protoFieldName")
    def getField: ByName = ???//TermName(s"get$protoFieldName")
    def getMapField: ByName = ???//TermName(s"get${protoFieldName}Map")
    def getListField: ByName = ???//TermName(s"get${protoFieldName}List")
    def builderSetter: ByName = if (isIterable(scalaTreeType)) addAllMethodName else setField

    val protoValueTree: Tree = {
//      if (builderType.member(addField) != NoSymbol) { // 有 addXXX 方法，说明 XXX 是一个数组
//        q"$protoIdent.$getListField"
//      } else if (builderType.member(putField) != NoSymbol) { // 有 putXXX 方法，说明 XXX 是一个 map
//        q"$protoIdent.$getMapField"
//      } else if (builderType.member(setField) != NoSymbol) { // 有 setXXX 方法
//        q"$protoIdent.$getField"
//      } else {
//        q""
//      }
      ???
    }

    lazy val protoValueSetterType: TypeRepr = {
//      val pbBuilderType = typeOf[com.google.protobuf.Message.Builder]
//      val setterOpt = if (builderType.member(addField) != NoSymbol) {
//        Some(builderType.member(addAllMethodName))
//      } else if (builderType.member(putField) != NoSymbol) {
//        Some(builderType.member(putAllMethodName))
//      } else if (builderType.member(setField) != NoSymbol) {
//        Some(builderType.member(setField))
//      } else {
//        None
//      }
//      setterOpt.fold(NoType)(_.alternatives.map(_.asMethod.paramLists.head.head.typeSignature).find(t ⇒ !(t <:< pbBuilderType)).get)
      ???
    }

    lazy val protoValueGetterType: TypeRepr = {
//      if (builderType.member(addField) != NoSymbol) { // 有 addXXX 方法，说明 XXX 是一个数组
//        protoType.member(getListField).typeSignature.resultType
//      } else if (builderType.member(putField) != NoSymbol) { // 有 putXXX 方法，说明 XXX 是一个 map
//        protoType.member(getMapField).typeSignature.resultType
//      } else if (builderType.member(setField) != NoSymbol) { // 有 setXXX 方法
//        protoType.member(getField).typeSignature.resultType
//      } else {
//        NoType
//      }
      ???
    }

  }



  // Convert scala clase class field value to protobuf value
  private case class ToProtoFieldProcessor(scalaEntity:Ident,accessor: Symbol)
      extends ToProtoProcessor(
        Apply(scalaEntity,List(accessor.asInstanceOf[Term])),
        ??? /*accessor.returnType*/,
        accessor.name.toString.capitalize)

  // Convert protobuf value to case class filed value
  private case class ToScalaFieldProcessor(accessor: Symbol)
      extends AbstractToScalaProcessor
      with ProtoFieldAnalyst {

    override def caseClassSelector: Symbol = accessor

    override def protoFieldName: String = accessor.name.toString.capitalize

    override def protoValueType: TypeRepr = protoValueGetterType

    override def optTree(optType: TypeRepr): Tree = {
      val hasProtoField = ByName( ??? /*s"has$protoFieldName"*/)
//      if (protoType.member(hasProtoField) == NoSymbol) {
//        defaultTree(optType)
//      } else {
//        q"$caseClassSelector = if (${protoIdent}.$hasProtoField) ${toScala(optType, protoValueType, protoValueTree)} else None"
//      }
      ???
    }
  }

  private[this] def resolveFieldType(originType: TypeRepr): TypeRepr = {
//    originType.asSeenFrom(entityType, entityType.typeSymbol.asClass)
    ???
  }

  private[this] def resolveType[T](using Type[T]): TypeRepr = {
//    val tpe = weakTypeOf[T]
//    tpe.dealias
//    ???
    TypeRepr.of[T]
  }

  private[this] def getCaseAccessors(): List[Symbol] = {
    val classSymbol = quotas.reflect.TypeReprMethods.classSymbol(scalaClassType).get
    quotas.reflect.SymbolMethods.caseFields(classSymbol)
  }

  private[this] def defaultProtoableFieldConvertTrees(scalaEntity:Ident): Map[String, Tree] = {
    getCaseAccessors().flatMap { a ⇒
      val p = ToProtoFieldProcessor(scalaEntity,a)
      p.tree.map(p.protoFieldName -> _)
    }.toMap
  }

  private[this] def protoableBody(protoableFieldConvertTrees: Iterable[Tree]): Tree = {
//    q"""
//      override def toProto(${entityIdent.name.toTermName}: $caseClassType): $protoType = {
//        val ${builderIdent} = ${protoType.typeSymbol.companion}.newBuilder()
//        ..$protoableFieldConvertTrees
//        $builderIdent.build()
//      }
//     """
    ???
  }

  private def defalutScalableFieldConvertTrees(): Map[String, Tree] = {
    getCaseAccessors().flatMap(accessor ⇒ ToScalaFieldProcessor(accessor).tree.map(accessor.name.toString -> _)).toMap
  }

  private def scalableBody(scalableFieldConvertTrees: Iterable[Tree]): Tree = {
//    q"""
//      override def toScala(${protoIdent.name.toTermName} :$protoType): $caseClassType = {
//        new ${caseClassType.typeSymbol} (
//           ..${scalableFieldConvertTrees}
//        )
//      }
//     """
    ???
  }

  def scalasImpl[T, M](using Type[T], Type[M]): Expr[Scalable[T,M]] = {
    val scalableFieldConvertTrees = defalutScalableFieldConvertTrees().values

    quotas.reflect.TreeMethods
      .asExpr(scalasImpl(scalableFieldConvertTrees, Nil)).asInstanceOf[Expr[Scalable[T,M]]]
  }

  private[this] def scalasImpl(scalableFieldConvertTrees: Iterable[Tree], perTrees: Iterable[Tree]): Tree = {
//    q"""
//       ..$perTrees
//       new $packageName.Scalable[$caseClassType, $protoType] { ${scalableBody(caseClassType, protoType, scalableFieldConvertTrees)} }
//    """
    ???
  }

  private def protosImpl: Expr[Protoable[T,M]] = {
    protosImpl(ident => defaultProtoableFieldConvertTrees(ident).values, Nil)
  }


  private[this] inline def protosImpl(protoableFieldConvertTrees:Ident => Iterable[Tree], perTrees: Iterable[Tree]): Expr[Protoable[T,M]] = {
//    q"""
//       ..$perTrees
//       new $packageName.Protoable[$caseClassType,$protoType] { ${protoableBody(caseClassType, protoType, protoableFieldConvertTrees)} }
//    """
    '{
      new Protoable[T,M] {
        override def toProto(scalaEntity: T): M = {
          val proto:M = ${newProtoTree}
          val builder = proto.toBuilder
//            ..$protoableFieldConvertTrees
//            builder.build()
          ${quotas.reflect.TreeMethods.asExprOf(Apply('{builder}.asInstanceOf[Term],List.empty[Term]).asInstanceOf[Nothing])}
        }
      }
    }
  }

  def convertsImpl[T, M](using Type[T], Type[M]): Expr[ProtoScalable[T,M]] = {
//    q"""
//      new $packageName.ProtoScalable[${resolveType[T]}, ${resolveType[M]}] {
//        ${scalableBody(caseClassType, protoType, defalutScalableFieldConvertTrees(caseClassType, protoType).values)}
//        ${protoableBody(caseClassType, protoType, defaultProtoableFieldConvertTrees(caseClassType, protoType).values)}
//      }
//    """
    ???
  }

  private def buildProtoableImpl[T, M]: Tree = {
//    val caseClassType = resolveType[T]
//    val protoType = resolveType[M]
//    val customTrees = MacroCache.builderFunctionTrees.getOrElse(getBuilderId(), mutable.Map.empty)
//    val (fixedCustomTrees, preTrees) = customTrees.map {
//      case (key, tree) ⇒
//        tree match { // setField
//          case buildFunction: Function ⇒
//            val functionName = TermName("builderFunction$" + MacroCache.getIdentityId)
//            val fromType = buildFunction.tpe.typeArgs.last // buildFunction 的返回值
//            val buildExpr = new ToProtoProcessor(q"$functionName($entityIdent)", fromType, caseClassType, protoType, key).tree.get
//            (key -> buildExpr) -> q"val $functionName = $buildFunction"
//          case value: Tree ⇒ // setFieldValue
//            val identity = TermName("identity$" + MacroCache.getIdentityId)
//            val buildExpr = new ToProtoProcessor(q"$identity", value.tpe, caseClassType, protoType, key).tree.get
//            (key -> buildExpr) -> q"val $identity = $value"
//        }
//    }.unzip
//    val protoableFieldConvertTrees = (defaultProtoableFieldConvertTrees(caseClassType, protoType) ++ fixedCustomTrees.toMap).values
//    protosImpl(caseClassType, protoType, protoableFieldConvertTrees, preTrees)
    ???
  }

  private def buildScalableImpl[T, M]: Tree = {
//    val caseClassType = resolveType[T]
//    val protoType = resolveType[M]
//    val builderId = getBuilderId()
//    val customTrees = MacroCache.builderFunctionTrees.getOrElse(builderId, mutable.Map.empty)
//    val entityType = resolveType[T]
//
//    val (fixedCustomTrees, preTrees) = customTrees.map {
//      case (key, tree) ⇒
//        val selector = entityType.member(TermName(key))
//        tree match {
//          case buildFunction: Function ⇒ // setField
//            val functionName = TermName("builderFunction$" + MacroCache.getIdentityId)
//            val expr = new ToScalaProcessor(selector.asMethod, q"$functionName($protoIdent)", buildFunction.body.tpe, resolveType[T], resolveType[M]).tree.get
//            (key -> expr) -> q"val $functionName = $buildFunction"
//          case value: Tree ⇒ // setFieldValue
//            val identity = TermName("identity$" + MacroCache.getIdentityId)
//            val expr = new ToScalaProcessor(selector.asMethod, q"$identity", value.tpe, resolveType[T], resolveType[M]).tree.get
//            (key -> expr) -> q"val $identity = $value"
//        }
//    }.unzip
//
//    val scalableFieldConvertTrees = (defalutScalableFieldConvertTrees(caseClassType, protoType) ++ fixedCustomTrees.toMap).values
//    scalasImpl(caseClassType, protoType, scalableFieldConvertTrees, preTrees)
    ???
  }

  private def setScalaFieldImpl[T, M, TF, MF](scalaField: Tree, value: Tree): Tree = {
//    val Function(_, Select(_, termName)) = scalaField
//    val builderId = getBuilderId()
//    MacroCache.builderFunctionTrees.getOrElseUpdate(builderId, mutable.Map.empty).update(termName.toString, value)
//    q"new ${c.prefix.actualType}"
    ???
  }

  private def setProtoFieldImpl[T, M, TF, MF](protoFieldSelector: Tree, value: Tree): Tree = {
//    val getter = "^get(\\w+)$".r
//    val listGetter = "^get(\\w+)List$".r
//    val mapGetter = "^get(\\w+)Map$".r
//    val setter = protoFieldSelector.find {
//      case _: Select ⇒ true
//      case _: Tree ⇒ false
//    } match {
//      case Some(select: Select) ⇒
//        select.name.toString match {
//          case mapGetter(n) if select.tpe.resultType <:< typeOf[java.util.Map[_, _]] ⇒ n
//          case listGetter(n) if select.tpe.resultType <:< typeOf[java.util.List[_]] ⇒ n
//          case getter(n) ⇒ n
//          case _ ⇒ c.abort(protoFieldSelector.pos, "Invalid setter")
//        }
//      case _ ⇒ c.abort(protoFieldSelector.pos, "Invalid setter")
//    }
//    val builderId = getBuilderId()
//    MacroCache.builderFunctionTrees.getOrElseUpdate(builderId, mutable.Map.empty).update(setter, value)
//    q"new ${c.prefix.actualType}"
    ???
  }

  val annoBuilderPrefix = "AnonBuilder$"

  private def scalableBuilderApply[T, M]: Tree = {
//    val className = TypeName(annoBuilderPrefix + MacroCache.getBuilderId)
//    q"""
//       class $className extends $packageName.ScalableBuilder[${resolveType[T]}, ${resolveType[M]}]
//       new $className
//     """
    ???
  }

  private def protoableBuilderApply[T, M]: Tree = {
//    val className = TypeName(annoBuilderPrefix + MacroCache.getBuilderId)
//    q"""
//       class $className extends $packageName.ProtoableBuilder[${resolveType[T]}, ${resolveType[M]}]
//       new $className
//     """
    ???
  }

  private[this] def getBuilderId() = {
//    c.prefix.actualType.toString.replace(annoBuilderPrefix, "").toInt
    quotas.reflect.Position.ofMacroExpansion
  }

}


object ProtoScalableMacro {

  inline def protoable[T,M<:GeneratedMessageV3]:Protoable[T,M] = {
    ${protosImpl[T,M]}
  }

  def protosImpl[T,M<:GeneratedMessageV3](using t:Type[T], m:Type[M],quotes: Quotes ):Expr[Protoable[T,M]] = {
    val protoScalableMacro = new ProtoScalableMacro(using t, m)
    protoScalableMacro.protosImpl
  }
}