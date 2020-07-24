package pbconverts

import scala.collection.mutable
import scala.reflect.macros.whitebox

// scalastyle:off number.of.methods
class ProtoScalableMacro(val c: whitebox.Context) {
  import c.universe._

  private[this] def packageName: Tree = q"_root_.pbconverts"
  private[this] def builderIdent = Ident(TermName("builder"))
  private[this] def entityIdent = Ident(TermName("entity"))
  private[this] def protoIdent = Ident(TermName("proto"))
  private[this] def implicitlyProtoable(entityType: Type, protoType: Type) = q"implicitly[$packageName.Protoable[$entityType,$protoType]]"

  private[this] def implicitlyScalable(entityType: Type, protoType: Type) = q"implicitly[$packageName.Scalable[$entityType,$protoType]]"

  private[this] def isOption(tpe: Type): Boolean = tpe <:< typeOf[Option[_]]

  private[this] def isIterable(tpe: Type): Boolean =
    (tpe <:< typeOf[Iterable[_]] || tpe <:< typeOf[Array[_]] || tpe <:< typeOf[java.lang.Iterable[_]]) && !(tpe <:< typeOf[Map[_, _]])

  private[this] def isMap(tpe: Type): Boolean = tpe <:< typeOf[Map[_, _]] || tpe <:< typeOf[java.util.Map[_, _]]

  private[this] def getBuilderType(protoType: Type): Type = {
    protoType.companion.member(TermName("newBuilder")).asTerm.alternatives.head.asMethod.returnType
  }

  trait Processor {

    def caseClassType: Type
    def protoType: Type

    def optTree(optFieldType: Type): Tree
    def seqTree(iterableType: Type): Tree
    def mapTree(mapType: Type): Tree
    def defaultTree(fieldType: Type): Tree

    def scalaTreeType: Type
    def protoValueType: Type

    def tree: Option[Tree] =
      Some {
        scalaTreeType match {
          case t: Type if isOption(t) ⇒
            optTree(resolveFieldType(caseClassType, t))
          case t: Type if isIterable(t) ⇒
            seqTree(resolveFieldType(caseClassType, t))
          case t: Type if isMap(t) ⇒
            mapTree(resolveFieldType(caseClassType, t))
          case t: Type ⇒
            val resolveType = resolveFieldType(caseClassType, t)
            defaultTree(resolveType)
        }
      }
  }

  class ToProtoProcessor(
      scalaTree: Tree,
      override val scalaTreeType: Type,
      override val caseClassType: Type,
      override val protoType: Type,
      override val protoFieldName: String
  ) extends ProtoFieldAnalyst {

    private[this] def toProto(t1: Type, t2: Type): Tree = {
      if (t1 <:< t2) {
        scalaTree
      } else {
        q"${implicitlyProtoable(t1, t2)}.toProto($scalaTree)"
      }
    }

    override def protoValueType: Type = protoValueSetterType

    override def optTree(optFieldType: Type): c.universe.Tree = {
      val tree = if (isIterable(protoValueType)) {
        seqTree(optFieldType)
      } else if (isMap(protoValueType)) {
        mapTree(optFieldType)
      } else {
        defaultTree(optFieldType)
      }
      If(q"$scalaTree.isDefined", tree, EmptyTree)
    }

    override def seqTree(iterableType: Type): c.universe.Tree = {
      val listType = builderType.member(addAllMethodName).asMethod.paramLists.head.head.typeSignature
      val valueTree = toProto(iterableType, listType)
      q"${builderIdent}.$addAllMethodName($valueTree)"
    }

    override def mapTree(mapType: Type): c.universe.Tree = {
      val putAllMapType = builderType.member(putAllMethodName).asMethod.paramLists.head.head.typeSignature
      val valueTree = toProto(mapType, putAllMapType)
      q"${builderIdent}.$putAllMethodName($valueTree)"
    }

    override def defaultTree(fieldType: Type): c.universe.Tree = {
      val valueTree = toProto(fieldType, protoValueType)
      q"${builderIdent}.$setField(${valueTree})"
    }

    override def tree: Option[Tree] = {
      if (scalaTreeType <:< typeOf[AnyRef]) {
        val notNullCondition = q"$scalaTree ne null"
        super.tree.map(If(notNullCondition, _, EmptyTree))
      } else {
        super.tree
      }
    }
  }

  case class ToScalaProcessor(caseClassSelector: MethodSymbol, protoValueTree: Tree, protoValueType: Type, caseClassType: Type, protoType: Type)
      extends AbstractToScalaProcessor

  trait AbstractToScalaProcessor extends Processor {

    def caseClassSelector: MethodSymbol

    def protoValueTree: Tree

    override def scalaTreeType: Type = caseClassSelector.returnType

    protected def toScala(t1: Type, t2: Type, value: Tree) = {
      if (t2 <:< t1) {
        value
      } else {
        q"${implicitlyScalable(t1, t2)}.toScala($value)"
      }
    }

    override def optTree(optType: Type): c.universe.Tree = defaultTree(optType)

    override def seqTree(iterableType: Type): Tree = defaultTree(iterableType)

    override def mapTree(mapType: c.universe.Type): Tree = defaultTree(mapType)

    override def defaultTree(fieldType: Type): Tree = {
      q"$caseClassSelector = ${toScala(fieldType, protoValueType, protoValueTree)}"
    }
  }

  trait ProtoFieldAnalyst extends Processor {
    def protoFieldName: String

    val builderType = getBuilderType(protoType)

    def addAllMethodName: TermName = TermName(s"addAll$protoFieldName")
    def putAllMethodName: TermName = TermName(s"putAll$protoFieldName")

    def addField: TermName = TermName(s"add$protoFieldName")
    def putField: TermName = TermName(s"put$protoFieldName")
    def setField: TermName = TermName(s"set$protoFieldName")
    def getField: TermName = TermName(s"get$protoFieldName")
    def getMapField: TermName = TermName(s"get${protoFieldName}Map")
    def getListField: TermName = TermName(s"get${protoFieldName}List")
    def builderSetter: TermName = if (isIterable(scalaTreeType)) addAllMethodName else setField

    val protoValueTree: Tree = {
      if (builderType.member(addField) != NoSymbol) { // 有 addXXX 方法，说明 XXX 是一个数组
        q"$protoIdent.$getListField"
      } else if (builderType.member(putField) != NoSymbol) { // 有 putXXX 方法，说明 XXX 是一个 map
        q"$protoIdent.$getMapField"
      } else if (builderType.member(setField) != NoSymbol) { // 有 setXXX 方法
        q"$protoIdent.$getField"
      } else {
        q""
      }
    }

    lazy val protoValueSetterType: Type = {
      val pbBuilderType = typeOf[com.google.protobuf.Message.Builder]
      val setterOpt = if (builderType.member(addField) != NoSymbol) {
        Some(builderType.member(addAllMethodName))
      } else if (builderType.member(putField) != NoSymbol) {
        Some(builderType.member(putAllMethodName))
      } else if (builderType.member(setField) != NoSymbol) {
        Some(builderType.member(setField))
      } else {
        None
      }
      setterOpt.fold(NoType)(_.alternatives.map(_.asMethod.paramLists.head.head.typeSignature).find(t ⇒ !(t <:< pbBuilderType)).get)
    }

    lazy val protoValueGetterType: Type = {
      if (builderType.member(addField) != NoSymbol) { // 有 addXXX 方法，说明 XXX 是一个数组
        protoType.member(getListField).typeSignature.resultType
      } else if (builderType.member(putField) != NoSymbol) { // 有 putXXX 方法，说明 XXX 是一个 map
        protoType.member(getMapField).typeSignature.resultType
      } else if (builderType.member(setField) != NoSymbol) { // 有 setXXX 方法
        protoType.member(getField).typeSignature.resultType
      } else {
        NoType
      }
    }

    override def tree: Option[Tree] = if (protoValueType != NoType) super.tree else None
  }

  case class ToProtoFieldProcessor(accessor: MethodSymbol, override val caseClassType: Type, override val protoType: Type)
      extends ToProtoProcessor(q"$entityIdent.$accessor", accessor.returnType, caseClassType, protoType, accessor.name.toString.capitalize)

  case class ToScalaFieldProcessor(accessor: MethodSymbol, override val caseClassType: Type, override val protoType: Type)
      extends AbstractToScalaProcessor
      with ProtoFieldAnalyst {

    override def caseClassSelector: MethodSymbol = accessor

    override def protoFieldName: String = accessor.name.toString.capitalize

    override def protoValueType: Type = protoValueGetterType

    override def optTree(optType: Type): c.universe.Tree = {
      val hasProtoField = TermName(s"has$protoFieldName")
      if (protoType.member(hasProtoField) == NoSymbol) {
        defaultTree(optType)
      } else {
        q"$caseClassSelector = if (${protoIdent}.$hasProtoField) ${toScala(optType, protoValueType, protoValueTree)} else None"
      }
    }
  }

  private[this] def resolveFieldType(entityType: Type, originType: Type): Type = {
    val types = (entityType.typeSymbol.asType.typeParams zip entityType.typeArgs).toMap
    def resolve(tpe: Type): Type = {
      if (tpe.typeArgs.isEmpty) {
        types.getOrElse(tpe.typeSymbol, tpe)
      } else {
        appliedType(tpe.typeConstructor, tpe.typeArgs.map(resolve))
      }
    }
    originType match {
      case t: Type if isOption(t) ⇒
        appliedType(typeOf[Option[_]].typeConstructor, resolve(t.typeArgs.head))
      case t: Type if isIterable(t) ⇒
        appliedType(t.typeConstructor, resolve(t.typeArgs.head))
      case t: Type if isMap(t) ⇒
        appliedType(t.typeConstructor, resolve(t.typeArgs.head), resolve(t.typeArgs.last))
      case t: Type ⇒
        resolve(t)
    }
  }

  private[this] def getCaseAccessors[T: WeakTypeTag]: Seq[MethodSymbol] = {
    weakTypeOf[T].members.collect { case m: MethodSymbol if m.isCaseAccessor ⇒ m }.toSeq.reverse
  }

  private[this] def defaultProtoableFieldConvertTrees[T: WeakTypeTag, M: WeakTypeTag]: Map[String, Tree] = {
    getCaseAccessors[T].flatMap { a ⇒
      val p = ToProtoFieldProcessor(a, weakTypeOf[T], weakTypeOf[M])
      p.tree.map(p.protoFieldName -> _)
    }.toMap
  }

  private[this] def protoableBody[T: WeakTypeTag, M: WeakTypeTag](protoableFieldConvertTrees: Iterable[Tree]): Tree = {
    val entityType = weakTypeOf[T]
    val protoType = weakTypeOf[M]
    q"""
      override def toProto(${entityIdent.name.toTermName}: $entityType): $protoType = {
        val ${builderIdent} = ${protoType.typeSymbol.companion}.newBuilder()
        ..$protoableFieldConvertTrees
        $builderIdent.build()
      }
     """
  }

  private def defalutScalableFieldConvertTrees[T: WeakTypeTag, M: WeakTypeTag]: Map[String, Tree] = {
    getCaseAccessors[T].flatMap(accessor ⇒ ToScalaFieldProcessor(accessor, weakTypeOf[T], weakTypeOf[M]).tree.map(accessor.name.toString -> _)).toMap
  }

  private def scalableBody[T: WeakTypeTag, M: WeakTypeTag](scalableFieldConvertTrees: Iterable[Tree]): Tree = {
    val entityType = weakTypeOf[T]
    val protoType = weakTypeOf[M]
    q"""
      override def toScala(${protoIdent.name.toTermName} :$protoType): $entityType = {
        new ${entityType.typeSymbol} (
           ..${scalableFieldConvertTrees}
        )
      }
     """
  }

  def scalasImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
    scalasImpl[T, M](defalutScalableFieldConvertTrees[T, M].values, Nil)
  }

  private[this] def scalasImpl[T: WeakTypeTag, M: WeakTypeTag](scalableFieldConvertTrees: Iterable[Tree], perTrees: Iterable[Tree]): Tree = {
    q"""
       ..$perTrees
       new $packageName.Scalable[${weakTypeOf[T]}, ${weakTypeOf[M]}] { ${scalableBody[T, M](scalableFieldConvertTrees)} }
    """
  }

  def protosImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
    protosImpl[T, M](defaultProtoableFieldConvertTrees[T, M].values, Nil)
  }

  private[this] def protosImpl[T: WeakTypeTag, M: WeakTypeTag](protoableFieldConvertTrees: Iterable[Tree], perTrees: Iterable[Tree]): Tree = {
    q"""
       ..$perTrees
       new $packageName.Protoable[${weakTypeOf[T]},${weakTypeOf[M]}] { ${protoableBody[T, M](protoableFieldConvertTrees)} }
    """
  }

  def convertsImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
    q"""
      new $packageName.ProtoScalable[${weakTypeOf[T]}, ${weakTypeOf[M]}] {
        ${scalableBody[T, M](defalutScalableFieldConvertTrees[T, M].values)}
        ${protoableBody[T, M](defaultProtoableFieldConvertTrees[T, M].values)}
      }
    """
  }

  def buildProtoableImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
    val customTrees = MacroCache.builderFunctionTrees.getOrElse(getBuilderId(), mutable.Map.empty)
    val (fixedCustomTrees, preTrees) = customTrees.map {
      case (key, tree) ⇒
        tree match { // setField
          case buildFunction: Function ⇒
            val functionName = TermName("builderFunction$" + MacroCache.getIdentityId)
            val fromType = buildFunction.tpe.typeArgs.last // buildFunction 的返回值
            val buildExpr = new ToProtoProcessor(q"$functionName($entityIdent)", fromType, weakTypeOf[T], weakTypeOf[M], key).tree.get
            (key -> buildExpr) -> q"val $functionName = $buildFunction"
          case value: Tree ⇒ // setFieldValue
            val identity = TermName("identity$" + MacroCache.getIdentityId)
            val buildExpr = new ToProtoProcessor(q"$identity", value.tpe, weakTypeOf[T], weakTypeOf[M], key).tree.get
            (key -> buildExpr) -> q"val $identity = $value"
        }
    }.unzip
    protosImpl[T, M]((defaultProtoableFieldConvertTrees[T, M] ++ fixedCustomTrees.toMap).values, preTrees)
  }

  def buildScalableImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
    val builderId = getBuilderId()
    val customTrees = MacroCache.builderFunctionTrees.getOrElse(builderId, mutable.Map.empty)
    val entityType = weakTypeOf[T]

    val (fixedCustomTrees, preTrees) = customTrees.map {
      case (key, tree) ⇒
        val selector = entityType.member(TermName(key))
        tree match {
          case buildFunction: Function ⇒ // setField
            val functionName = TermName("builderFunction$" + MacroCache.getIdentityId)
            val expr = new ToScalaProcessor(selector.asMethod, q"$functionName($protoIdent)", buildFunction.body.tpe, weakTypeOf[T], weakTypeOf[M]).tree.get
            (key -> expr) -> q"val $functionName = $buildFunction"
          case value: Tree ⇒ // setFieldValue
            val identity = TermName("identity$" + MacroCache.getIdentityId)
            val expr = new ToScalaProcessor(selector.asMethod, q"$identity", value.tpe, weakTypeOf[T], weakTypeOf[M]).tree.get
            (key -> expr) -> q"val $identity = $value"
        }
    }.unzip

    scalasImpl[T, M]((defalutScalableFieldConvertTrees[T, M] ++ fixedCustomTrees.toMap).values, preTrees)
  }

  def setScalaFieldImpl[T: WeakTypeTag, M: WeakTypeTag, TF: WeakTypeTag, MF: WeakTypeTag](scalaField: Tree, value: Tree): Tree = {
    val Function(_, Select(_, termName)) = scalaField
    val builderId = getBuilderId()
    MacroCache.builderFunctionTrees.getOrElseUpdate(builderId, mutable.Map.empty).update(termName.toString, value)
    q"new ${c.prefix.actualType}"
  }

  def setProtoFieldImpl[T: WeakTypeTag, M: WeakTypeTag, TF: WeakTypeTag, MF: WeakTypeTag](protoFieldSelector: Tree, value: Tree): Tree = {
    val getter = "^get(\\w+)$".r
    val listGetter = "^get(\\w+)List$".r
    val mapGetter = "^get(\\w+)Map$".r
    val setter = protoFieldSelector.find {
      case _: Select ⇒ true
      case _: Tree ⇒ false
    } match {
      case Some(select: Select) ⇒
        select.name.toString match {
          case mapGetter(n) if select.tpe.resultType <:< typeOf[java.util.Map[_, _]] ⇒ n
          case listGetter(n) if select.tpe.resultType <:< typeOf[java.util.List[_]] ⇒ n
          case getter(n) ⇒ n
          case _ ⇒ c.abort(protoFieldSelector.pos, "Invalid setter")
        }
      case _ ⇒ c.abort(protoFieldSelector.pos, "Invalid setter")
    }
    val builderId = getBuilderId()
    MacroCache.builderFunctionTrees.getOrElseUpdate(builderId, mutable.Map.empty).update(setter, value)
    q"new ${c.prefix.actualType}"
  }

  val annoBuilderPrefix = "AnonBuilder$"

  def scalableBuilderApply[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
    val className = TypeName(annoBuilderPrefix + MacroCache.getBuilderId)
    q"""
       class $className extends $packageName.ScalableBuilder[${weakTypeOf[T]}, ${weakTypeOf[M]}]
       new $className
     """
  }

  def protoableBuilderApply[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
    val className = TypeName(annoBuilderPrefix + MacroCache.getBuilderId)
    q"""
       class $className extends $packageName.ProtoableBuilder[${weakTypeOf[T]}, ${weakTypeOf[M]}]
       new $className
     """
  }

  private[this] def getBuilderId() = {
    c.prefix.actualType.toString.replace(annoBuilderPrefix, "").toInt
  }

}
