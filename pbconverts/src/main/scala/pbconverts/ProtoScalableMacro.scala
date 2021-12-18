package pbconverts

import com.google.protobuf.MessageLite.Builder
import com.google.protobuf.{ByteString, GeneratedMessageV3, Message}
import quoted.{Expr, Quotes, Type}

import scala.collection.mutable
import scala.quoted.ToExpr.NoneToExpr
import scala.quoted.Exprs

// scalastyle:off number.of.methods
class ProtoScalableMacro[T: Type, M <: Message: Type](using quotas: Quotes) {
  import quotas.reflect._

  private val scalaClassType = quotas.reflect.TypeRepr.of[T].dealias
  private val protoClassType = quotas.reflect.TypeRepr.of[M].dealias

  private val scalaClassSymbol: quotas.reflect.Symbol = scalaClassType.typeSymbol
  private val protoClassSymbol: quotas.reflect.Symbol = protoClassType.typeSymbol

  private val protoCompanionIdent = Ref(protoClassSymbol.companionModule)
  private val newBuilder = Select.overloaded(protoCompanionIdent, "newBuilder", Nil, Nil)
  private val builderType: TypeRepr = newBuilder.tpe
  private val builderClassSymbol: Symbol = builderType.typeSymbol

  private def builderBuildeWithTerms(termsBuilder: Ident => List[Term]) = {
    ValDef.let(Symbol.spliceOwner, "builder", newBuilder) { builderIdent =>
      val terms: List[Term] = termsBuilder(builderIdent.asInstanceOf[Ident])
      Block(terms, Select.overloaded(builderIdent, "build", Nil, Nil))
    }
  }

  private[this] def implicitlyProtoable(entityType: TypeRepr, protoType: TypeRepr): Term = {
    val target = TypeRepr.of[Protoable].appliedTo(List(entityType, protoType))
    Implicits.search(target) match {
      case s: ImplicitSearchSuccess => s.tree
      case _                        => report.errorAndAbort(s"implicit ${Printer.TypeReprAnsiCode.show(target)} not found")
    }
  }

  private[this] def implicitlyScalable(entityType: TypeRepr, protoType: TypeRepr): Term = {
    val target = TypeRepr.of[Scalable].appliedTo(List(entityType, protoType))
    Implicits.search(target) match {
      case s: ImplicitSearchSuccess => s.tree
      case _                        => report.errorAndAbort(s"implicit ${Printer.TypeReprAnsiCode.show(target)} not found")
    }
  }

  private[this] def isOption(tpe: TypeRepr): Boolean = {
    tpe <:< TypeRepr.of[Option[_]]
  }

  private[this] def isIterable(tpe: TypeRepr): Boolean =
    (tpe <:< TypeRepr.of[Iterable[_]]
      || tpe <:< TypeRepr.of[Array[_]]
      || tpe <:< TypeRepr.of[java.lang.Iterable[_]]) && !(tpe <:< TypeRepr.of[Map[_, _]] || tpe <:< TypeRepr.of[ByteString])

  private[this] def isMap(tpe: TypeRepr): Boolean = tpe <:< TypeRepr.of[Map[_, _]] || tpe <:< TypeRepr.of[java.util.Map[_, _]]

  private trait Processor {

    def scalaTreeType: TypeRepr
    def protoValueType: TypeRepr

    def resolvedCaseClassFieldType: TypeRepr = resolveFieldType(scalaTreeType)

    def tree: Option[Term]
  }

  // Convert expr (`scalaTree` in this class) to protobuf value
  private class ToProtoProcessor(
      protoBuilderIdent: Ident,
      scalaTree: Term,
      override val scalaTreeType: TypeRepr,
      override val protoFieldName: String
  ) extends ProtoFieldAnalyst {

    override def protoIdent: ProtoScalableMacro.this.quotas.reflect.Ident = ???

    private[this] def toProto(t1: TypeRepr, t2: TypeRepr): Term = {
      if (t1 <:< t2) {
        scalaTree
      } else {
        val toProtoMethod = TypeRepr.of[Protoable[_, _]].typeSymbol.memberMethod("toProto").head
        Apply(Select(implicitlyProtoable(t1, t2), toProtoMethod), List(scalaTree))
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
      If(Select.unique(scalaTree, "isDefined"), tree, '{}.asTerm)
    }

    def seqTree(iterableType: TypeRepr): Term = {
      val addAllMethod = builderClassSymbol.memberMethod(addAllMethodName).last
      val listType = addAllMethod.tree.asInstanceOf[DefDef].paramss.head.params.head.asInstanceOf[ValDef].tpt.tpe
      val valueTree = toProto(iterableType, listType)
      Select.overloaded(protoBuilderIdent, addAllMethodName, Nil, valueTree :: Nil)
    }

    def mapTree(mapType: TypeRepr): Term = {
      val putAllMethod = builderClassSymbol.memberMethod(putAllMethodName).head
      val putAllMapType = putAllMethod.tree.asInstanceOf[DefDef].paramss.head.params.head.asInstanceOf[ValDef].tpt.tpe
      val valueTree = toProto(mapType, putAllMapType)
      Select.overloaded(protoBuilderIdent, putAllMethodName, Nil, valueTree :: Nil)
    }

    def defaultTree(fieldType: TypeRepr): Term = {
      val valueTree = toProto(fieldType, protoValueType)
      Select.overloaded(protoBuilderIdent, setField, Nil, valueTree :: Nil)
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

        scalaTree match {
          case Select(_, _) if scalaTreeType <:< TypeRepr.of[AnyRef] =>
            val cond = Select.unique(scalaTree, "ne").appliedTo(Literal(NullConstant()))
            Some(If(cond, _tree, '{}.asTerm))
          case _ =>
            Some(_tree)
        }
      }
    }
  }

  // Convert expr (`protoValueTree` in this class) to case class filed value
  private case class ToScalaProcessor(caseClassSelector: Symbol, protoValueTree: Term, protoValueType: TypeRepr) extends AbstractToScalaProcessor

  private trait AbstractToScalaProcessor extends Processor {

    def caseClassSelector: Symbol

    def protoValueTree: Term

    override def scalaTreeType: TypeRepr = caseClassSelector.tree.asInstanceOf[ValDef].tpt.tpe

    protected def toScala(t1: TypeRepr, t2: TypeRepr, value: Term): Term = {
      if (t2 <:< t1) {
        value
      } else {
        val toScalaMethod = TypeRepr.of[Scalable[_, _]].typeSymbol.memberMethod("toScala").head
        Apply(Select(implicitlyScalable(t1, t2), toScalaMethod), List(value))
      }
    }

    def optTree(optType: TypeRepr): Term = defaultTree(optType)

    def defaultTree(fieldType: TypeRepr): Term = {
      NamedArg(caseClassSelector.name, toScala(fieldType, protoValueType, protoValueTree))
    }

    override def tree: Option[Term] =
      if (protoValueType == TypeRepr.of[Unit]) {
        None
      } else {
        val tree = if (isOption(scalaTreeType)) {
          optTree(resolvedCaseClassFieldType)
        } else {
          defaultTree(resolvedCaseClassFieldType)
        }
        Some(tree)
      }
  }

  // Contain some methods and vals to analyze Protobuf fields
  private trait ProtoFieldAnalyst extends Processor {

    def protoIdent: Ident

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
        Apply(Select(protoIdent, builderClassSymbol.memberMethod(getListField).head), Nil)
      } else if (builderClassSymbol.memberMethod(putField).nonEmpty) {
        Apply(Select(protoIdent, builderClassSymbol.memberMethod(getMapField).head), Nil)
      } else if (builderClassSymbol.memberMethod(setField).nonEmpty) {
        Apply(Select(protoIdent, builderClassSymbol.memberMethod(getField).head), Nil)
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
      setters
        .map(_.tree.asInstanceOf[DefDef].paramss.head.params.head.asInstanceOf[ValDef].tpt.tpe)
        .filterNot(_ <:< pbBuilderType)
        .headOption
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
  private case class ToProtoFieldProcessor(scalaEntity: Ident, protoBuilderIdent: Ident, accessor: Symbol)
      extends ToProtoProcessor(
        protoBuilderIdent: Ident,
        Select(scalaEntity, accessor),
        scalaClassType.memberType(accessor),
        accessor.name.toString.capitalize
      ) {
    override def protoIdent: ProtoScalableMacro.this.quotas.reflect.Ident = ???
  }

  // Convert protobuf value to case class filed value
  private case class ToScalaFieldProcessor(protoIdent: Ident, accessor: Symbol) extends AbstractToScalaProcessor with ProtoFieldAnalyst {

    override def caseClassSelector: Symbol = accessor

    override def protoFieldName: String = accessor.name.toString.capitalize

    override def protoValueType: TypeRepr = protoValueGetterType.getOrElse(TypeRepr.of[Unit])

    override def optTree(optType: TypeRepr): Term = {
      val hasProtoField = s"has$protoFieldName"
      val hasProtoFieldMethod = protoClassSymbol.memberMethod(hasProtoField).headOption
      hasProtoFieldMethod match {
        case Some(method) =>
          val AppliedType(_, typeArg :: Nil) = optType
          If(
            Apply(Select(protoIdent, method), Nil),
            toScala(optType, protoValueType, protoValueTree),
            '{ None }.asTerm
          )
        case None => defaultTree(optType)
      }
    }
  }

  private[this] def resolveFieldType(originType: TypeRepr): TypeRepr = {
    val typeTree = scalaClassType.typeSymbol.tree.asInstanceOf[TypeDef]

    val types = scalaClassType match {
      case AppliedType(typeConstructor, args) => scalaClassType.typeSymbol.typeMembers.zip(args).toMap
      case _                                  => Map.empty
    }

    def resolve(tpe: TypeRepr): TypeRepr = {
      val typeMembers = tpe.typeSymbol.typeMembers
      if (typeMembers.isEmpty) {
        types.getOrElse(tpe.typeSymbol, tpe)
      } else {
        tpe match {
          case AppliedType(typeConstructor, typeArgs) =>
            typeConstructor.appliedTo(typeArgs.map(resolve))
          case other => other
        }
      }
    }

    originType match {
      case AppliedType(tycon, args) ⇒
        tycon.appliedTo(args.map(resolve))
      case t: TypeRepr ⇒
        resolve(t)
    }
  }

  private[this] def getCaseAccessors(): List[Symbol] = {
    val classSymbol = quotas.reflect.TypeReprMethods.classSymbol(scalaClassType).get
    quotas.reflect.SymbolMethods.caseFields(classSymbol)
  }

  private[this] def defaultProtoableFieldConvertTrees(scalaEntity: Ident, protoBuilderIdent: Ident): List[Tuple2[String, Term]] = {
    getCaseAccessors().flatMap { a ⇒
      val p = ToProtoFieldProcessor(scalaEntity, protoBuilderIdent, a)
      p.tree.map(p.protoFieldName -> _)
    }
  }

  private[this] def protoableBody(scalaEntityIdent: Ident, protoableFieldConvertTrees: (Ident, Ident) => List[Term]): Expr[M] = {
    builderBuildeWithTerms(protoableFieldConvertTrees(scalaEntityIdent, _)).asExprOf[M]
  }

  private def defalutScalableFieldConvertTrees(protoIdent: Ident): Map[String, Term] = {
    getCaseAccessors().flatMap(accessor ⇒ ToScalaFieldProcessor(protoIdent, accessor).tree.map(accessor.name.toString -> _)).toMap
  }

  private def scalableBody(protoIdent: Ident, scalableFieldConvertTrees: Ident => Iterable[Term]): Expr[T] = {
    val args = scalableFieldConvertTrees(protoIdent).toList

    val types = scalaClassType match {
      case t: AppliedType => t.args
      case _              => Nil
    }

    val scalaCompanionIdent = Ref(scalaClassType.typeSymbol.companionModule)
    Select.overloaded(scalaCompanionIdent, "apply", types, args).asExprOf[T]
  }

  def scalasImpl: Expr[Scalable[T, M]] = {
    scalasImpl { protoIdent =>
      val args = defalutScalableFieldConvertTrees(protoIdent)
      getCaseAccessors().flatMap(a => args.get(a.name))
    }
  }

  private[this] def scalasImpl(scalableFieldConvertTrees: Ident => List[Term]): Expr[Scalable[T, M]] = {
    '{
      new Scalable[T, M] {
        def toScala(proto: M): T = ${ scalableBody('{ proto }.asTerm.asInstanceOf[Ident], scalableFieldConvertTrees) }
      }
    }
  }

  private def protosImpl: Expr[Protoable[T, M]] = {
    protosImpl((scalaEntityIdent, protoBuilderIdent) => defaultProtoableFieldConvertTrees(scalaEntityIdent, protoBuilderIdent).map(_._2))
  }

  private[this] def protosImpl(protoableFieldConvertTrees: (Ident, Ident) => List[Term]): Expr[Protoable[T, M]] = {
    '{
      new Protoable[T, M] {
        def toProto(scalaEntity: T): M = ${ protoableBody('{ scalaEntity }.asTerm.asInstanceOf[Ident], protoableFieldConvertTrees) }
      }
    }
  }

  def protoScalableImpl: Expr[ProtoScalable[T, M]] = {
    '{
      new ProtoScalable[T, M] {
        def toScala(proto: M) = ${ scalableBody('{ proto }.asTerm.asInstanceOf[Ident], defalutScalableFieldConvertTrees(_).values) }

        def toProto(scalaEntity: T): M = ${
          val scalaEntityIdent = '{ scalaEntity }.asTerm.asInstanceOf[Ident]
          builderBuildeWithTerms(defaultProtoableFieldConvertTrees(scalaEntityIdent, _).map(_._2)).asExprOf[M]
        }
      }
    }
  }

  private def buildProtoableImpl: Expr[Protoable[T, M]] = {
    val customTrees = MacroCache.builderFunctionTrees.getOrElse(getBuilderId(), mutable.Map.empty)
    def getCustomTrees(scalaIdent: Ident, protoBuilderIdent: Ident): Map[String, Term] = customTrees.map { case (key, tree) ⇒
      tree match { // setField
        case buildFunction: Expr[_] if buildFunction.isExprOf[_ => _] ⇒
          val tree = buildFunction.asTerm
          val applied = Apply(Select.unique(tree, "apply"), List(scalaIdent))
          val buildExpr = new ToProtoProcessor(protoBuilderIdent, applied, applied.tpe, key).tree.get

          (key -> buildExpr)
        case value: Expr[_] ⇒ // setFieldValue
          val valueTerm = value.asTerm
          val buildExpr = new ToProtoProcessor(protoBuilderIdent, valueTerm, valueTerm.tpe, key).tree.get
          (key -> buildExpr)
      }
    }.toMap
    protosImpl({ (scalaIdent, protoBuilderIdent) =>
      val terms = getCustomTrees(scalaIdent, protoBuilderIdent)
      (defaultProtoableFieldConvertTrees(scalaIdent, protoBuilderIdent) ++ terms).toMap.values.toList
    })
  }

  private def buildScalableImpl: Expr[Scalable[T, M]] = {
    val builderId = getBuilderId()
    val customTrees = MacroCache.builderFunctionTrees.getOrElse(builderId, mutable.Map.empty)

    def getCustomTrees(protoIdent: Ident): Map[String, Term] = customTrees.map { case (key, tree) ⇒
      tree match {
        case buildFunction: Expr[_] if buildFunction.isExprOf[_ => _] ⇒ // setField
          val selector = scalaClassSymbol.caseFields.find(_.name == key).get
          val tree = buildFunction.asTerm
          val applied = Apply(Select.unique(tree, "apply"), List(protoIdent))
          val expr = new ToScalaProcessor(selector, applied, applied.tpe).tree.get
          (key -> expr)
        case value: Expr[_] ⇒ // setFieldValue
          val valueTerm = value.asTerm
          val selector = scalaClassSymbol.caseFields.find(_.name == key).get
          val buildExpr = new ToScalaProcessor(selector, valueTerm, valueTerm.tpe).tree.get
          (key -> buildExpr)
      }
    }.toMap
    scalasImpl { protoIdent =>
      val defaults = defalutScalableFieldConvertTrees(protoIdent)
      val customs = getCustomTrees(protoIdent)
      val all = defaults ++ customs
      getCaseAccessors().flatMap { accessor =>
        all.get(accessor.name)
      }
    }
  }

  private def setScalaFieldImpl[TF, MF](scalaField: Expr[T => TF], value: Expr[(M => MF) | MF]): Expr[ScalableBuilder[T, M]] = {
    scalaField.asTerm match {
      case Inlined(_, _, Inlined(_, _, Block(List(DefDef(_, _, _, Some(Select(_, symbolName)))), _))) =>
        val builderId = getBuilderId()
        MacroCache.builderFunctionTrees.getOrElseUpdate(builderId, mutable.Map.empty).update(symbolName, value)
        scalableBuilderApply
      case _ => report.errorAndAbort("Invalid setter")
    }
  }

  private def setProtoFieldImpl[TF: Type, MF: Type](protoFieldSelector: Expr[M ⇒ MF], value: Expr[(T ⇒ TF) | TF]): Expr[ProtoableBuilder[T, M]] = {
    val getter = "^get(\\w+)$".r
    val listGetter = "^get(\\w+)List$".r
    val mapGetter = "^get(\\w+)Map$".r
    val setter = protoFieldSelector.asTerm match {
      case Inlined(_, _, Inlined(_, _, Block(List(DefDef(_, _, _, Some(Apply(Select(qualifier, symbolName), _)))), _))) =>
        symbolName match {
          case mapGetter(n) if TypeRepr.of[MF] <:< TypeRepr.of[java.util.Map[_, _]] ⇒ n
          case listGetter(n) if TypeRepr.of[MF] <:< TypeRepr.of[java.util.List[_]] ⇒ n
          case getter(n) ⇒ n
          case _ => report.errorAndAbort("Invalid field selector")
        }
      case _ => report.errorAndAbort("Invalid field selector")
    }

    val builderId = Symbol.spliceOwner.owner.fullName
    MacroCache.builderFunctionTrees.getOrElseUpdate(builderId, mutable.Map.empty).update(setter, value)
    protoableBuilderApply
  }

  val annoBuilderPrefix = "AnonBuilder$"

  private def scalableBuilderApply: Expr[ScalableBuilder[T, M]] = {
    '{ ScalableBuilder._default.asInstanceOf[ScalableBuilder[T, M]] }
  }

  private def protoableBuilderApply: Expr[ProtoableBuilder[T, M]] = {
    '{ ProtoableBuilder._default.asInstanceOf[ProtoableBuilder[T, M]] }
  }

  private[this] def getBuilderId(): String = {
    Symbol.spliceOwner.owner.fullName
  }

}

object ProtoScalableMacro {

  inline def protoable[T, M <: Message]: Protoable[T, M] = ${ protosImpl[T, M] }

  def protosImpl[T: Type, M <: Message: Type](using quotes: Quotes): Expr[Protoable[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.protosImpl
  }

  inline def protoableBuilder[T, M <: Message]: ProtoableBuilder[T, M] = ${ protoableBuilderImpl[T, M] }

  def protoableBuilderImpl[T: Type, M <: Message: Type](using quotes: Quotes): Expr[ProtoableBuilder[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.protoableBuilderApply
  }

  inline def protoableBuilderSetField[T, M <: Message, MF, TF](inline protoFieldSelector: M ⇒ MF, inline value: (T ⇒ TF) | TF): ProtoableBuilder[T, M] =
    ${ protoableBuilderSetFieldImpl[T, M, MF, TF]('protoFieldSelector, 'value) }

  def protoableBuilderSetFieldImpl[T: Type, M <: Message: Type, MF: Type, TF: Type](protoFieldSelector: Expr[M ⇒ MF], value: Expr[(T ⇒ TF) | TF])(using
      quotes: Quotes
  ): Expr[ProtoableBuilder[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.setProtoFieldImpl[TF, MF](protoFieldSelector, value)
  }

  inline def scalableBuilder[T, M <: Message]: ScalableBuilder[T, M] = ${ scalableBuilderImpl[T, M] }

  def scalableBuilderImpl[T: Type, M <: Message: Type](using quotes: Quotes): Expr[ScalableBuilder[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.scalableBuilderApply
  }

  inline def scalableBuilderSetField[T, M <: Message, TF, MF](inline scalaFieldSelector: T ⇒ TF, inline value: (M ⇒ MF) | MF): ScalableBuilder[T, M] =
    ${ scalableBuilderSetFieldImpl[T, M, TF, MF]('scalaFieldSelector, 'value) }

  def scalableBuilderSetFieldImpl[T: Type, M <: Message: Type, TF: Type, MF: Type](scalaFieldSelector: Expr[T ⇒ TF], value: Expr[(M ⇒ MF) | MF])(using
      quotes: Quotes
  ): Expr[ScalableBuilder[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.setScalaFieldImpl[TF, MF](scalaFieldSelector, value)
  }

  inline def buildScalable[T, M <: Message]: Scalable[T, M] =
    ${ buildScalableImpl[T, M] }

  def buildScalableImpl[T: Type, M <: Message: Type](using quotes: Quotes): Expr[Scalable[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.buildScalableImpl
  }

  inline def buildProtoable[T, M <: Message]: Protoable[T, M] =
    ${ buildProtoableImpl[T, M] }

  def buildProtoableImpl[T: Type, M <: Message: Type](using quotes: Quotes): Expr[Protoable[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.buildProtoableImpl
  }

  inline def scalable[T, M <: Message]: Scalable[T, M] = ${ scalasImpl[T, M] }

  def scalasImpl[T: Type, M <: Message: Type](using quotes: Quotes): Expr[Scalable[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.scalasImpl
  }

  inline def protoScalable[T, M <: Message]: ProtoScalable[T, M] = ${ protoScalableImpl[T, M] }

  def protoScalableImpl[T: Type, M <: Message: Type](using quotes: Quotes): Expr[ProtoScalable[T, M]] = {
    val protoScalableMacro = new ProtoScalableMacro[T, M]
    protoScalableMacro.protoScalableImpl
  }
}
