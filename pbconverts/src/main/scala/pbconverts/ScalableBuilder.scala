package pbconverts

import com.google.protobuf.Message

class ScalableBuilder[+T, M]() {

  def setField[TF, MF](scalaField: T ⇒ TF, value: M ⇒ MF): ScalableBuilder[T, M] = ??? // macro ProtoScalableMacro.setScalaFieldImpl[T, M, TF, MF]

  def setFieldValue[TF, MF](scalaField: T ⇒ TF, value: MF): ScalableBuilder[T, M] = ??? // macro ProtoScalableMacro.setScalaFieldImpl[T, M, TF, MF]

  def build: Scalable[T, M] = ??? // macro ProtoScalableMacro.buildScalableImpl[T, M]
}

object ScalableBuilder {
  inline def apply[T, M <: Message]: ScalableBuilder[T, M] = ProtoScalableMacro.scalableBuilder[T, M]
}
