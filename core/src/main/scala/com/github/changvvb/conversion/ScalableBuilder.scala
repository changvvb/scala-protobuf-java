package com.github.changvvb.conversion

class ScalableBuilder[+T, M]() {

  def setField[TF, MF](scalaField: T ⇒ TF, value: M ⇒ MF): ScalableBuilder[T, M] = macro ProtoScalableMacro.setScalaFieldImpl[T, M, TF, MF]

  def setFieldValue[TF, MF](scalaField: T ⇒ TF, value: MF): ScalableBuilder[T, M] = macro ProtoScalableMacro.setScalaFieldImpl[T, M, TF, MF]

  def build: Scalable[T, M] = macro ProtoScalableMacro.buildScalableImpl[T, M]
}

object ScalableBuilder {
  def apply[T, M]: ScalableBuilder[T, M] = macro ProtoScalableMacro.scalableBuilderApply[T, M]
}
