//package pbconverts
//
//import com.google.protobuf.ByteString
//
//import scala.collection.mutable
//import scala.reflect.macros.whitebox
//import scala.quoted
//import _root_.quoted.Expr
//
//// scalastyle:off number.of.methods
//object ProtoScalableMacro {
//
//  private[this] def packageName: Tree = q"_root_.pbconverts"
//  private[this] def builderIdent = Ident(TermName("builder"))
//  private[this] def entityIdent = Ident(TermName("entity"))
//  private[this] def protoIdent = Ident(TermName("proto"))
//  private[this] def implicitlyProtoable(entityType: Type, protoType: Type) = q"implicitly[$packageName.Protoable[$entityType,$protoType]]"
//
//  private[this] def implicitlyScalable(entityType: Type, protoType: Type) = q"implicitly[$packageName.Scalable[$entityType,$protoType]]"
//
//  private[this] def isOption(tpe: Type): Boolean = tpe <:< typeOf[Option[_]]
//
//  private[this] def isIterable(tpe: Type): Boolean =
//    (tpe <:< typeOf[Iterable[_]]
//      || tpe <:< typeOf[Array[_]]
//      || tpe <:< typeOf[java.lang.Iterable[_]]) && !(tpe <:< typeOf[Map[_, _]] || tpe <:< typeOf[ByteString])
//
//  private[this] def isMap(tpe: Type): Boolean = tpe <:< typeOf[Map[_, _]] || tpe <:< typeOf[java.util.Map[_, _]]
//
//  private[this] def getBuilderType(protoType: Type): Type = {
//    protoType.companion.member(TermName("newBuilder")).asTerm.alternatives.head.asMethod.returnType
//  }
//
//  trait Processor {
//
//    def caseClassType: Type
//    def protoType: Type
//
//    def scalaTreeType: Type
//    def protoValueType: Type
//
//    def resolvedCaseClassFieldType: Type = resolveFieldType(caseClassType, scalaTreeType)
//
//    def tree: Option[Tree]
//  }
//
//  // Convert expr (`scalaTree` in this class) to protobuf value
//  class ToProtoProcessor(
//      scalaTree: Tree,
//      override val scalaTreeType: Type,
//      override val caseClassType: Type,
//      override val protoType: Type,
//      override val protoFieldName: String
//  ) extends ProtoFieldAnalyst {
//
//    private[this] def toProto(t1: Type, t2: Type): Tree = {
//      if (t1 <:< t2) {
//        scalaTree
//      } else {
//        q"${implicitlyProtoable(t1, t2)}.toProto($scalaTree)"
//      }
//    }
//
//    override def protoValueType: Type = protoValueSetterType
//
//    def optTree(optFieldType: Type): c.universe.Tree = {
//      val tree = if (isIterable(protoValueType)) {
//        seqTree(optFieldType)
//      } else if (isMap(protoValueType)) {
//        mapTree(optFieldType)
//      } else {
//        defaultTree(optFieldType)
//      }
//      If(q"$scalaTree.isDefined", tree, EmptyTree)
//    }
//
//    def seqTree(iterableType: Type): c.universe.Tree = {
//      val listType = builderType.member(addAllMethodName).asMethod.paramLists.head.head.typeSignature
//      val valueTree = toProto(iterableType, listType)
//      q"${builderIdent}.$addAllMethodName($valueTree)"
//    }
//
//    def mapTree(mapType: Type): c.universe.Tree = {
//      val putAllMapType = builderType.member(putAllMethodName).asMethod.paramLists.head.head.typeSignature
//      val valueTree = toProto(mapType, putAllMapType)
//      q"${builderIdent}.$putAllMethodName($valueTree)"
//    }
//
//    def defaultTree(fieldType: Type): c.universe.Tree = {
//      val valueTree = toProto(fieldType, protoValueType)
//      q"${builderIdent}.$setField(${valueTree})"
//    }
//
//    override def tree: Option[Tree] = {
//      if (protoValueType == NoType) {
//        None
//      } else {
//        val _tree = if (isOption(scalaTreeType)) {
//          optTree(resolvedCaseClassFieldType)
//        } else if (isIterable(protoValueType)) {
//          seqTree(resolvedCaseClassFieldType)
//        } else if (isMap(protoValueType)) {
//          mapTree(resolvedCaseClassFieldType)
//        } else {
//          defaultTree(resolvedCaseClassFieldType)
//        }
//
//        if (scalaTreeType <:< typeOf[AnyRef]) {
//          val notNullCondition = q"$scalaTree ne null"
//          Some(If(notNullCondition, _tree, EmptyTree))
//        } else {
//          Some(_tree)
//        }
//      }
//    }
//  }
//
//  // Convert expr (`protoValueTree` in this class) to case class filed value
//  case class ToScalaProcessor(caseClassSelector: MethodSymbol, protoValueTree: Tree, protoValueType: Type, caseClassType: Type, protoType: Type)
//      extends AbstractToScalaProcessor
//
//  trait AbstractToScalaProcessor extends Processor {
//
//    def caseClassSelector: MethodSymbol
//
//    def protoValueTree: Tree
//
//    override def scalaTreeType: Type = caseClassSelector.returnType
//
//    protected def toScala(t1: Type, t2: Type, value: Tree) = {
//      if (t2 <:< t1) {
//        value
//      } else {
//        q"${implicitlyScalable(t1, t2)}.toScala($value)"
//      }
//    }
//
//    def optTree(optType: Type): c.universe.Tree = defaultTree(optType)
//
//    def defaultTree(fieldType: Type): Tree = {
//      q"$caseClassSelector = ${toScala(fieldType, protoValueType, protoValueTree)}"
//    }
//
//    override def tree: Option[Tree] =
//      if (protoValueType == NoType) {
//        None
//      } else {
//        if (isOption(scalaTreeType)) {
//          Some(optTree(resolvedCaseClassFieldType))
//        } else {
//          Some(defaultTree(resolvedCaseClassFieldType))
//        }
//      }
//  }
//
//  // Contain some methods and vals to analyze Protobuf fields
//  trait ProtoFieldAnalyst extends Processor {
//    def protoFieldName: String
//
//    val builderType = getBuilderType(protoType)
//
////    def addAllMethodName: TermName = TermName(s"addAll$protoFieldName")
////    def putAllMethodName: TermName = TermName(s"putAll$protoFieldName")
////
////    def addField: TermName = TermName(s"add$protoFieldName")
////    def putField: TermName = TermName(s"put$protoFieldName")
////    def setField: TermName = TermName(s"set$protoFieldName")
////    def getField: TermName = TermName(s"get$protoFieldName")
////    def getMapField: TermName = TermName(s"get${protoFieldName}Map")
////    def getListField: TermName = TermName(s"get${protoFieldName}List")
////    def builderSetter: TermName = if (isIterable(scalaTreeType)) addAllMethodName else setField
//
//    val protoValueTree: Tree = {
////      if (builderType.member(addField) != NoSymbol) { // 有 addXXX 方法，说明 XXX 是一个数组
////        q"$protoIdent.$getListField"
////      } else if (builderType.member(putField) != NoSymbol) { // 有 putXXX 方法，说明 XXX 是一个 map
////        q"$protoIdent.$getMapField"
////      } else if (builderType.member(setField) != NoSymbol) { // 有 setXXX 方法
////        q"$protoIdent.$getField"
////      } else {
////        q""
////      }
//      ???
//    }
//
//    lazy val protoValueSetterType: TypeRepr = {
////      val pbBuilderType = typeOf[com.google.protobuf.Message.Builder]
////      val setterOpt = if (builderType.member(addField) != NoSymbol) {
////        Some(builderType.member(addAllMethodName))
////      } else if (builderType.member(putField) != NoSymbol) {
////        Some(builderType.member(putAllMethodName))
////      } else if (builderType.member(setField) != NoSymbol) {
////        Some(builderType.member(setField))
////      } else {
////        None
////      }
////      setterOpt.fold(NoType)(_.alternatives.map(_.asMethod.paramLists.head.head.typeSignature).find(t ⇒ !(t <:< pbBuilderType)).get)
//      ???
//    }
//
//    lazy val protoValueGetterType: TypeRepr = {
////      if (builderType.member(addField) != NoSymbol) { // 有 addXXX 方法，说明 XXX 是一个数组
////        protoType.member(getListField).typeSignature.resultType
////      } else if (builderType.member(putField) != NoSymbol) { // 有 putXXX 方法，说明 XXX 是一个 map
////        protoType.member(getMapField).typeSignature.resultType
////      } else if (builderType.member(setField) != NoSymbol) { // 有 setXXX 方法
////        protoType.member(getField).typeSignature.resultType
////      } else {
////        NoType
////      }
//      ???
//    }
//
//  }
//
//  // Convert scala clase class field value to protobuf value
//  case class ToProtoFieldProcessor(accessor: MethodSymbol, override val caseClassType: Type, override val protoType: Type)
//      extends ToProtoProcessor(q"$entityIdent.$accessor", accessor.returnType, caseClassType, protoType, accessor.name.toString.capitalize)
//
//  // Convert protobuf value to case class filed value
//  case class ToScalaFieldProcessor(accessor: MethodSymbol, override val caseClassType: Type, override val protoType: Type)
//      extends AbstractToScalaProcessor
//      with ProtoFieldAnalyst {
//
//    override def caseClassSelector: MethodSymbol = accessor
//
//    override def protoFieldName: String = accessor.name.toString.capitalize
//
//    override def protoValueType: Type = protoValueGetterType
//
//    override def optTree(optType: Type): c.universe.Tree = {
//      val hasProtoField = TermName(s"has$protoFieldName")
//      if (protoType.member(hasProtoField) == NoSymbol) {
//        defaultTree(optType)
//      } else {
//        q"$caseClassSelector = if (${protoIdent}.$hasProtoField) ${toScala(optType, protoValueType, protoValueTree)} else None"
//      }
//    }
//  }
//
//  private[this] def resolveFieldType(entityType: Type, originType: Type): Type =
//    originType.asSeenFrom(entityType, entityType.typeSymbol.asClass)
//
//  private[this] def resolveType[T: WeakTypeTag]: Type = {
//    val tpe = weakTypeOf[T]
//    tpe.dealias
//  }
//
//  private[this] def getCaseAccessors(caseClassType: TypeRepr): Seq[Symbole] = {
//    caseClassType.members.collect { case m: MethodSymbol if m.isCaseAccessor ⇒ m }.toSeq.reverse
//  }
//
//  private[this] def defaultProtoableFieldConvertTrees(caseClassType: TypeRepr, protoType: TypeRepr): Map[String, Tree] = {
//    getCaseAccessors(caseClassType).flatMap { a ⇒
//      val p = ToProtoFieldProcessor(a, caseClassType, protoType)
//      p.tree.map(p.protoFieldName -> _)
//    }.toMap
//  }
//
//  private[this] def protoableBody(caseClassType: TypeRepr, protoType: TypeRepr, protoableFieldConvertTrees: Iterable[Tree]): Tree = {
////    q"""
////      override def toProto(${entityIdent.name.toTermName}: $caseClassType): $protoType = {
////        val ${builderIdent} = ${protoType.typeSymbol.companion}.newBuilder()
////        ..$protoableFieldConvertTrees
////        $builderIdent.build()
////      }
////     """
//    ???
//  }
//
//  private def defalutScalableFieldConvertTrees(caseClassType: Type, protoType: Type): Map[String, Tree] = {
//    getCaseAccessors(caseClassType).flatMap(accessor ⇒ ToScalaFieldProcessor(accessor, caseClassType, protoType).tree.map(accessor.name.toString -> _)).toMap
//  }
//
//  private def scalableBody(caseClassType: Type, protoType: Type, scalableFieldConvertTrees: Iterable[Tree]): Tree = {
//    q"""
//      override def toScala(${protoIdent.name.toTermName} :$protoType): $caseClassType = {
//        new ${caseClassType.typeSymbol} (
//           ..${scalableFieldConvertTrees}
//        )
//      }
//     """
//  }
//
//  def scalasImpl[T, M]: Expr[Scalable] = {
//    val caseClassType = resolveType[T]
//    val protoType = resolveType[M]
//    val scalableFieldConvertTrees = defalutScalableFieldConvertTrees(caseClassType, protoType).values
//    scalasImpl(caseClassType, protoType, scalableFieldConvertTrees, Nil)
//  }
//
//  private[this] def scalasImpl(caseClassType: Type, protoType: Type, scalableFieldConvertTrees: Iterable[Tree], perTrees: Iterable[Tree]): Tree = {
//    q"""
//       ..$perTrees
//       new $packageName.Scalable[$caseClassType, $protoType] { ${scalableBody(caseClassType, protoType, scalableFieldConvertTrees)} }
//    """
//  }
//
//  def protosImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
//    val caseClassType = resolveType[T]
//    val protoType = resolveType[M]
//    protosImpl(caseClassType, protoType, defaultProtoableFieldConvertTrees(caseClassType, protoType).values, Nil)
//  }
//
//  private[this] def protosImpl(caseClassType: Type, protoType: Type, protoableFieldConvertTrees: Iterable[Tree], perTrees: Iterable[Tree]): Tree = {
//    q"""
//       ..$perTrees
//       new $packageName.Protoable[$caseClassType,$protoType] { ${protoableBody(caseClassType, protoType, protoableFieldConvertTrees)} }
//    """
//  }
//
//  def convertsImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
//    val caseClassType = resolveType[T]
//    val protoType = resolveType[M]
//    q"""
//      new $packageName.ProtoScalable[${resolveType[T]}, ${resolveType[M]}] {
//        ${scalableBody(caseClassType, protoType, defalutScalableFieldConvertTrees(caseClassType, protoType).values)}
//        ${protoableBody(caseClassType, protoType, defaultProtoableFieldConvertTrees(caseClassType, protoType).values)}
//      }
//    """
//  }
//
//  def buildProtoableImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
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
//  }
//
//  def buildScalableImpl[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
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
//  }
//
//  def setScalaFieldImpl[T: WeakTypeTag, M: WeakTypeTag, TF: WeakTypeTag, MF: WeakTypeTag](scalaField: Tree, value: Tree): Tree = {
//    val Function(_, Select(_, termName)) = scalaField
//    val builderId = getBuilderId()
//    MacroCache.builderFunctionTrees.getOrElseUpdate(builderId, mutable.Map.empty).update(termName.toString, value)
//    q"new ${c.prefix.actualType}"
//  }
//
//  def setProtoFieldImpl[T: WeakTypeTag, M: WeakTypeTag, TF: WeakTypeTag, MF: WeakTypeTag](protoFieldSelector: Tree, value: Tree): Tree = {
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
//  }
//
//  val annoBuilderPrefix = "AnonBuilder$"
//
//  def scalableBuilderApply[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
//    val className = TypeName(annoBuilderPrefix + MacroCache.getBuilderId)
//    q"""
//       class $className extends $packageName.ScalableBuilder[${resolveType[T]}, ${resolveType[M]}]
//       new $className
//     """
//  }
//
//  def protoableBuilderApply[T: WeakTypeTag, M: WeakTypeTag]: Tree = {
//    val className = TypeName(annoBuilderPrefix + MacroCache.getBuilderId)
//    q"""
//       class $className extends $packageName.ProtoableBuilder[${resolveType[T]}, ${resolveType[M]}]
//       new $className
//     """
//  }
//
//  private[this] def getBuilderId() = {
//    c.prefix.actualType.toString.replace(annoBuilderPrefix, "").toInt
//  }
//
//}
