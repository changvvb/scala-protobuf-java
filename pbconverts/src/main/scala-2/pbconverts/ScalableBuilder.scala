package pbconverts

class ScalableBuilder[+S, P]() {

  def setField[SF, PF](scalaField: S ⇒ SF, value: P ⇒ PF): ScalableBuilder[S, P] = macro ProtoScalableMacro.setScalaFieldImpl[S, P, SF, PF]

  def setFieldValue[SF, PF](scalaField: S ⇒ SF, value: PF): ScalableBuilder[S, P] = macro ProtoScalableMacro.setScalaFieldImpl[S, P, SF, PF]

  def build: Scalable[S, P] = macro ProtoScalableMacro.buildScalableImpl[S, P]
}

object ScalableBuilder {
  def apply[S, P]: ScalableBuilder[S, P] = macro ProtoScalableMacro.scalableBuilderApply[S, P]
}
