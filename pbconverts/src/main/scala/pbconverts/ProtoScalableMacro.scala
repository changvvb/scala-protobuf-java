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
  private[this] val protoClassSymbolClass = quotas.reflect.SymbolMethods.companionClass(protoClassSymbol)
  private[this] val protoClassSymbolModule = quotas.reflect.SymbolMethods.companionModule(protoClassSymbol)

  private val newBuilderMethod = quotas.reflect.SymbolMethods.memberMethod(protoClassSymbolModule)("newBuilder").last
  private val builderType:TypeRepr = newBuilderMethod.tree.asInstanceOf[DefDef].returnTpt.tpe
  private val builderClassSymbol:Symbol = builderType.classSymbol.get
  private val builderBuildMethod = builderType.classSymbol.get.memberMethod("build").head

  private val TypeRef(outer, name) = protoClassType.asInstanceOf[TypeRef]
  private val protoCompanionIdent = Ident.apply(TermRef.apply(outer, name))
  private val newBuilderApply = Apply(Select.apply(protoCompanionIdent, newBuilderMethod),List.empty)
  private def builderBuildeWithTerms(termsBuilder:Ident => List[Term]) = ValDef.let(Symbol.spliceOwner,"builder",newBuilderApply) { ident =>
    val terms = termsBuilder.apply(ident)
    Block(terms,Apply(Select(ident,builderBuildMethod),Nil))

  }

//  private val builderBuildExpr = Apply(ident,)
//  private[this] def packageName  = ???//q"_root_.pbconverts"
//  private[this] def builderIdent = ???//Ident(TermRef("builder"))
//  private[this] def entityIdent = ??? //Ident.apply(TermRef(,"entity"))  //Ident(TermRef("entity"))
//  private[this] def protoIdent = ??? //Ident(TermRef("proto"))
  private[this] def implicitlyProtoable(entityType: TypeRepr, protoType: TypeRepr):Term = {
  val target = TypeRepr.of[Protoable].appliedTo(List(entityType,protoType))
  Implicits.search(target) match {
    case s: ImplicitSearchSuccess => s.tree
    case _ =>
      report.error(s"implicit ${Printer.TypeReprAnsiCode.show(target)} not found")
      ???
  }
}

  private[this] def implicitlyScalable(entityType: TypeRepr, protoType: TypeRepr):Term = {
    Implicits.search(TypeApply('{Scalable}.asTerm,List(TypeIdent(entityType.typeSymbol),TypeIdent(protoType.typeSymbol))).tpe) match {
      case s:ImplicitSearchSuccess => s.tree
      case _ => ???
    }
  } //q"implicitly[$packageName.Scalable[$entityType,$protoType]]"

  private[this] def isOption(tpe: TypeRepr): Boolean = {
    tpe <:< TypeRepr.of[Option[_]]
  }

  private[this] def isIterable(tpe: TypeRepr): Boolean =
    (tpe <:< TypeRepr.of[Iterable[_]]
      || tpe <:< TypeRepr.of[Array[_]]
      || tpe <:< TypeRepr.of[java.lang.Iterable[_]]) && !(tpe <:< TypeRepr.of[Map[_, _]] || tpe <:< TypeRepr.of[ByteString])

  private[this] def isMap(tpe: TypeRepr): Boolean = tpe <:< TypeRepr.of[Map[_, _]] || tpe <:< TypeRepr.of[java.util.Map[_, _]]

  private[this] def getBuilderType(): TypeRepr = {
    builderType
  }

  private trait Processor {

    def scalaTreeType: TypeRepr
    def protoValueType: TypeRepr

    def resolvedCaseClassFieldType: TypeRepr = resolveFieldType(scalaTreeType)

    def tree: Option[Tree]
  }

  // Convert expr (`scalaTree` in this class) to protobuf value
  private class ToProtoProcessor(
                                  protoBuilderIdent:Ident,
                                  scalaTree: Term,
      override val scalaTreeType: TypeRepr,
      override val protoFieldName: String
  ) extends ProtoFieldAnalyst {

    private[this] def toProto(t1: TypeRepr, t2: TypeRepr): Term = {
      if (t1 <:< t2) {
        scalaTree
      } else {
        val toProtoMethod = TypeRepr.of[Protoable[_,_]].typeSymbol.memberMethod("toProto").head
        Apply(Select(implicitlyProtoable(t1,t2),toProtoMethod),List(scalaTree))
      }
    }

    override def protoValueType: TypeRepr = protoValueSetterType.getOrElse(TypeRepr.of[Unit])

    def optTree(optFieldType: TypeRepr): Term = {
      val tree = if (isIterable(protoValueType)) {
        seqTree(optFieldType)
      } else if (isMap(protoValueType)) {
        mapTree(optFieldType)
      } else {
        defaultTree(optFieldType)
      }
      '{ if(${scalaTree.asExprOf[Option[_]]}.isDefined) ${tree.asExpr} }.asTerm
//      If(q"$scalaTree.isDefined", tree, EmptyTree)
    }

    def seqTree(iterableType: TypeRepr): Term = {
      val addAllMethod =  builderClassSymbol.memberMethod(addAllMethodName).last
      val listType = addAllMethod.tree.asInstanceOf[DefDef].paramss.head.params.head.asInstanceOf[ValDef].tpt.tpe
      val valueTree = toProto(iterableType, listType)
      Apply(Select(protoBuilderIdent, addAllMethod), List(valueTree))
    }

    def mapTree(mapType: TypeRepr): Term = {
      val putAllMethod =  builderClassSymbol.memberMethod(putAllMethodName).head
      val putAllMapType = putAllMethod.tree.asInstanceOf[DefDef].paramss.head.params.head.asInstanceOf[ValDef].tpt.tpe
      val valueTree = toProto(mapType, putAllMapType)
      Apply(Select(protoBuilderIdent, putAllMethod), List(valueTree))
    }

    def defaultTree(fieldType: TypeRepr): Term = {
      val valueTree = toProto(fieldType, protoValueType)
      val setFieldMethod =  builderClassSymbol.memberMethod(setField).last
      Apply(Select(protoBuilderIdent, setFieldMethod), List(valueTree))
    }

    override def tree: Option[Term] = {
      if (protoValueType == TypeRepr.of[Unit]) {
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
           Some('{
            if(${scalaTree.asExprOf[AnyRef]} ne null) ${_tree.asExpr}
          }.asTerm)
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

    def addAllMethodName: String = s"addAll$protoFieldName"
    def putAllMethodName: String = s"putAll$protoFieldName"

    def addField: String = s"add$protoFieldName"
    def putField: String = s"put$protoFieldName"
    def setField: String = s"set$protoFieldName"
    def getField: String = s"get$protoFieldName"
    def getMapField: String = s"get${protoFieldName}Map"
    def getListField: String = s"get${protoFieldName}List"
    def builderSetter: String = if (isIterable(scalaTreeType)) addAllMethodName else setField

    def protoValueTree: Tree = {


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

    lazy val protoValueSetterType: Option[TypeRepr] = {
      val setters = if (builderClassSymbol.memberMethod(addField).nonEmpty) {
        builderClassSymbol.memberMethod(addAllMethodName)
      } else if (builderClassSymbol.memberMethod(putField).nonEmpty) {
        builderClassSymbol.memberMethod(putAllMethodName)
      } else if (builderClassSymbol.memberMethod(setField).nonEmpty) {
        builderClassSymbol.memberMethod(setField)
      } else {
        Nil
      }
      val pbBuilderType = TypeRepr.of[com.google.protobuf.Message.Builder]
      setters.map(_.tree.asInstanceOf[DefDef].paramss.head.params.head.asInstanceOf[ValDef].tpt.tpe)
        .filterNot(_ <:< pbBuilderType).headOption
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
  private case class ToProtoFieldProcessor(scalaEntity:Ident,protoBuilderIdent:Ident,accessor: Symbol)
      extends ToProtoProcessor(
        protoBuilderIdent:Ident,
        Select(scalaEntity,accessor),
        scalaClassType.memberType(accessor),
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
    originType
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

  private[this] def defaultProtoableFieldConvertTrees(scalaEntity:Ident, protoBuilderIdent:Ident): List[Tuple2[String, Term]] = {
    getCaseAccessors().flatMap { a ⇒
      val p = ToProtoFieldProcessor(scalaEntity, protoBuilderIdent,a)
      p.tree.map(Printer.TreeAnsiCode.show).foreach(println(_))
      p.tree.map(p.protoFieldName -> _)
    }
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
    protosImpl((scalaEntityIdent, protoBuilderIdent) => defaultProtoableFieldConvertTrees(scalaEntityIdent, protoBuilderIdent).map(_._2), Nil)
  }


  private[this] def protosImpl(protoableFieldConvertTrees:(Ident, Ident) => List[Term], perTrees: Iterable[Tree]): Expr[Protoable[T,M]] = {
    val expr = '{
      new Protoable[T,M] {
        override def toProto(scalaEntity: T): M = ${
          val scalaEntityIdent = '{scalaEntity}.asTerm.asInstanceOf[Ident]
          val x= builderBuildeWithTerms(protoableFieldConvertTrees(scalaEntityIdent,_))
          println(x)
          x.asExprOf[M]
        }
      }
    }
    println(expr.show)
    expr
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

  def protosImpl[T,M<:GeneratedMessageV3](using t:Type[T], m:Type[M],quotes: Quotes):Expr[Protoable[T,M]] = {
    val protoScalableMacro = new ProtoScalableMacro(using t, m)
    protoScalableMacro.protosImpl
  }
}