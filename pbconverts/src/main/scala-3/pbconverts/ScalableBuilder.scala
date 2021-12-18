package pbconverts

import com.google.protobuf.Message

class ScalableBuilder[+T, M <: Message]() {

  inline def setField[TF, MF](inline scalaField: T ⇒ TF, inline value: M ⇒ MF): ScalableBuilder[T, M] =
    ProtoScalableMacro.scalableBuilderSetField[T, M, TF, MF](scalaField, value)

  inline def setFieldValue[TF, MF](inline scalaField: T ⇒ TF, inline value: MF): ScalableBuilder[T, M] =
    ProtoScalableMacro.scalableBuilderSetField[T, M, TF, MF](scalaField, value)

  inline def build: Scalable[T, M] = ProtoScalableMacro.buildScalable[T, M]
}

object ScalableBuilder {
  val _default = new ScalableBuilder[Nothing, Nothing]
  inline def apply[T, M <: Message]: ScalableBuilder[T, M] = ProtoScalableMacro.scalableBuilder[T, M]
}
