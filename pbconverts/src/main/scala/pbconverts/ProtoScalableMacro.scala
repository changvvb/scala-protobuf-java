package pbconverts

import com.google.protobuf.MessageLite.Builder
import com.google.protobuf.{ByteString, GeneratedMessageV3, Message}
import quoted.{Expr, Quotes, Type}

import scala.collection.mutable

// scalastyle:off number.of.methods
class ProtoScalableMacro[T,M](using typeT:Type[T], typeM:Type[M])(using quotas:Quotes) {
  import quotas.reflect._

  private[this] val scalaClassType = quotas.reflect.TypeRepr.of[T](using typeT).dealias
  private[this] val protoClassType = quotas.reflect.TypeRepr.of[M](using typeM).dealias

  private[this] val scalaClassSymbol = scalaClassType.typeSymbol
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
    val terms = termsBuilder(ident.asInstanceOf[Ident])
    Block(terms,Apply(Select(ident,builderBuildMethod),Nil))
  }

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
    val target = TypeRepr.of[Scalable].appliedTo(List(entityType, protoType))
    Implicits.search(target) match {
      case s: ImplicitSearchSuccess => s.tree
      case _ =>
        report.error(s"implicit ${Printer.TypeReprAnsiCode.show(target)} not found")
        ???
    } //q"implicitly[$packageName.Scalable[$entityType,$protoType]]"
  }

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

    def tree: Option[Term]
  }

  // Convert expr (`scalaTree` in this class) to protobuf value
  private class ToProtoProcessor(
                                  protoBuilderIdent:Ident,
                                  scalaTree: Term,
      override val scalaTreeType: TypeRepr,
      override val protoFieldName: String
  ) extends ProtoFieldAnalyst {


    override def protoIdent: ProtoScalableMacro.this.quotas.reflect.Ident = ???

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
  private case class ToScalaProcessor(caseClassSelector: Symbol, protoValueTree: Term, protoValueType: TypeRepr)
      extends AbstractToScalaProcessor

  private trait AbstractToScalaProcessor extends Processor {

    def caseClassSelector: Symbol

    def protoValueTree: Term

    override def scalaTreeType: TypeRepr =  caseClassSelector.tree.asInstanceOf[ValDef].tpt.tpe

    protected def toScala(t1: TypeRepr, t2: TypeRepr, value: Term):Term = {
      if (t2 <:< t1) {
        value
      } else {
        val toScalaMethod = TypeRepr.of[Scalable[_,_]].typeSymbol.memberMethod("toScala").head
        Apply(Select(implicitlyScalable(t1,t2),toScalaMethod),List(value))
      }
    }

    def optTree(optType: TypeRepr): Term = defaultTree(optType)

    def defaultTree(fieldType: TypeRepr): Term = {
//      q"$caseClassSelector = ${toScala(fieldType, protoValueType, protoValueTree)}"
      Assign(caseClassSelector.tree.asInstanceOf[Term],toScala(fieldType,protoValueType,protoValueTree))
    }

    override def tree: Option[Term] =
      if (protoValueType == TypeRepr.of[Unit]) {
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

    def protoIdent:Ident

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

    def protoValueTree: Term = {
      if (builderClassSymbol.memberMethod(addField).nonEmpty) {
        Apply(Select(protoIdent,builderClassSymbol.memberMethod(getListField).head),Nil)
      } else if (builderClassSymbol.memberMethod(putField).nonEmpty) {
        Apply(Select(protoIdent,builderClassSymbol.memberMethod(getMapField).head),Nil)
      } else if (builderClassSymbol.memberMethod(setField).nonEmpty) {
        Apply(Select(protoIdent,builderClassSymbol.memberMethod(getField).head),Nil)
      } else {
        '{}.asTerm
      }
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

    lazy val protoValueGetterType: Option[TypeRepr] = {
      val getters = if (builderClassSymbol.memberMethod(addField).nonEmpty) {
        builderClassSymbol.memberMethod(getListField)
      } else if (builderClassSymbol.memberMethod(putField).nonEmpty) {
        builderClassSymbol.memberMethod(getMapField)
      } else if (builderClassSymbol.memberMethod(setField).nonEmpty) {
        builderClassSymbol.memberMethod(getField)
      } else {
        Nil
      }
      getters.headOption.map(_.tree.asInstanceOf[DefDef].returnTpt.tpe)
    }

  }


  // Convert scala clase class field value to protobuf value
  private case class ToProtoFieldProcessor(scalaEntity:Ident,protoBuilderIdent:Ident,accessor: Symbol)
      extends ToProtoProcessor(
        protoBuilderIdent:Ident,
        Select(scalaEntity,accessor),
        scalaClassType.memberType(accessor),
        accessor.name.toString.capitalize) {
    override def protoIdent: ProtoScalableMacro.this.quotas.reflect.Ident = ???
  }

  // Convert protobuf value to case class filed value
  private case class ToScalaFieldProcessor(protoIdent:Ident, accessor: Symbol)
      extends AbstractToScalaProcessor
      with ProtoFieldAnalyst {

    override def caseClassSelector: Symbol = accessor

    override def protoFieldName: String = accessor.name.toString.capitalize

    override def protoValueType: TypeRepr = protoValueGetterType.getOrElse(TypeRepr.of[Unit])

    override def optTree(optType: TypeRepr): Term = {
      val hasProtoField = s"has$protoFieldName"
      val hasProtoFieldMethod = protoClassSymbol.memberMethod(hasProtoField).headOption
      hasProtoFieldMethod match {
        case Some(method) =>
          val optExpr = '{
            if(${Apply(Select(protoIdent,method),Nil).asExprOf[Boolean]}) {
              Some(${toScala(optType, protoValueType, protoValueTree).asExpr})
            } else {
              None
            }
          }
          Assign(accessor.tree.asInstanceOf[Term],optExpr.asTerm)
        case None => defaultTree(optType)
      }
    }
  }

  private[this] def resolveFieldType(originType: TypeRepr): TypeRepr = {
    val typeTree = scalaClassType.typeSymbol.tree.asInstanceOf[TypeDef]
//    println(quotas.reflect.TypeReprMethods.widen(originType))
//    println(typeTree.rhs)



//    val types = (entityType.typeSymbol.asType.typeParams zip entityType.typeArgs).toMap
//    def resolve(tpe: Type): Type = {
//      if (tpe.typeArgs.isEmpty) {
//        types.getOrElse(tpe.typeSymbol, tpe)
//      } else {
//        appliedType(tpe.typeConstructor, tpe.typeArgs.map(resolve))
//      }
//    }
//    originType match {
//      case t: Type if isOption(t) ⇒
//        appliedType(typeOf[Option[_]].typeConstructor, resolve(t.typeArgs.head))
//      case t: Type if isIterable(t) ⇒
//        appliedType(t.typeConstructor, resolve(t.typeArgs.head))
//      case t: Type if isMap(t) ⇒
//        appliedType(t.typeConstructor, resolve(t.typeArgs.head), resolve(t.typeArgs.last))
//      case t: Type ⇒
//        resolve(t)
//    }

     println(Printer.TreeAnsiCode.show(typeTree))

//    originType.asSeenFrom(entityType, entityType.typeSymbol.asClass)
//    TypeReprMethods.asType(originType)
//    originType.select(scalaClassSymbol)
//    originType

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

  private def defalutScalableFieldConvertTrees(protoIdent:Ident): Map[String, Term] = {
    getCaseAccessors().flatMap(accessor ⇒ ToScalaFieldProcessor(protoIdent,accessor).tree.map(accessor.name.toString -> _)).toMap
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

  def scalasImpl: Expr[Scalable[T,M]] = {
      scalasImpl(defalutScalableFieldConvertTrees(_).values, Nil)
  }

  private[this] def scalasImpl(scalableFieldConvertTrees: Ident => Iterable[Term], perTrees: Iterable[Tree]): Expr[Scalable[T,M]] = {
//    q"""
//       ..$perTrees
//       new $packageName.Scalable[$caseClassType, $protoType] { ${scalableBody(caseClassType, protoType, scalableFieldConvertTrees)} }
//    """

    val res = '{
      new Scalable[T,M] {
        override def toScala(proto: M): T = ${
          val protoIdent = '{proto}.asTerm.asInstanceOf[Ident]
          val x= scalableFieldConvertTrees(protoIdent)
          x.foreach { t =>
//            println(Printer.TreeAnsiCode.show(t))
          }
          New(TypeTree.of[T]).asExprOf[T]
        }
      }
    }
    quotas.show(res)
    res
  }

  private def protosImpl: Expr[Protoable[T,M]] = {
    protosImpl((scalaEntityIdent, protoBuilderIdent) => defaultProtoableFieldConvertTrees(scalaEntityIdent, protoBuilderIdent).map(_._2), Nil)
  }


  private[this] def protosImpl(protoableFieldConvertTrees:(Ident, Ident) => List[Term], perTrees: Iterable[Tree]): Expr[Protoable[T,M]] = {
    '{
      new Protoable[T,M] {
        override def toProto(scalaEntity: T): M = ${
          val scalaEntityIdent = '{scalaEntity}.asTerm.asInstanceOf[Ident]
          builderBuildeWithTerms(protoableFieldConvertTrees(scalaEntityIdent,_)).asExprOf[M]
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

  inline def protoable[T,M]:Protoable[T,M] = ${protosImpl[T,M]}

  def protosImpl[T,M](using t:Type[T], m:Type[M],quotes: Quotes):Expr[Protoable[T,M]] = {
    val protoScalableMacro = new ProtoScalableMacro(using t, m)
    protoScalableMacro.protosImpl
  }

  inline def scalable[T,M]:Scalable[T,M] = ${scalasImpl[T,M]}

  def scalasImpl[T,M](using t:Type[T], m:Type[M],quotes: Quotes):Expr[Scalable[T,M]] = {
    val protoScalableMacro = new ProtoScalableMacro(using t, m)
    protoScalableMacro.scalasImpl
  }
}