package pbconverts

import com.google.protobuf.Message

class ScalableBuilder[+S, P <: Message]() {

  inline def setField[SF, PF](inline scalaField: S ⇒ SF, inline value: P ⇒ PF): ScalableBuilder[S, P] =
    ProtoScalableMacro.scalableBuilderSetField[S, P, SF, PF](scalaField, value)

  inline def setFieldValue[SF, PF](inline scalaField: S ⇒ SF, inline value: PF): ScalableBuilder[S, P] =
    ProtoScalableMacro.scalableBuilderSetField[S, P, SF, PF](scalaField, value)

  inline def build: Scalable[S, P] = ProtoScalableMacro.buildScalable[S, P]
}

object ScalableBuilder {
  val _default = new ScalableBuilder[Nothing, Nothing]
  inline def apply[S, P <: Message]: ScalableBuilder[S, P] = ProtoScalableMacro.scalableBuilder[S, P]
}
