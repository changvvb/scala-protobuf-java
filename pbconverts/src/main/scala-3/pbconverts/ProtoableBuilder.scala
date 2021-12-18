package pbconverts

import com.google.protobuf.Message

class ProtoableBuilder[T, M <: Message]() {

  inline def setField[TF, MF](inline protoFieldSelector: M ⇒ MF, inline value: T ⇒ TF): ProtoableBuilder[T, M] =
    ProtoScalableMacro.protoableBuilderSetField[T, M, MF, TF](protoFieldSelector, value)

  inline def setFieldValue[TF, MF](inline protoFieldSelector: M ⇒ MF, inline value: TF): ProtoableBuilder[T, M] =
    ProtoScalableMacro.protoableBuilderSetField[T, M, MF, TF](protoFieldSelector, value)

  inline def build: Protoable[T, M] = ProtoScalableMacro.buildProtoable[T, M]
}

object ProtoableBuilder {
  val _default = new ProtoableBuilder[Nothing, Nothing]
  inline def apply[T, M <: Message]: ProtoableBuilder[T, M] = ProtoScalableMacro.protoableBuilder[T, M]
}
