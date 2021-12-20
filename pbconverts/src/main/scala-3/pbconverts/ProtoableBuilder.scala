package pbconverts

import com.google.protobuf.Message

class ProtoableBuilder[S, P <: Message]() {

  inline def setField[SF, PF](inline protoFieldSelector: P ⇒ PF, inline value: S ⇒ SF): ProtoableBuilder[S, P] =
    ProtoScalableMacro.protoableBuilderSetField[S, P, PF, SF](protoFieldSelector, value)

  inline def setFieldValue[SF, PF](inline protoFieldSelector: P ⇒ PF, inline value: SF): ProtoableBuilder[S, P] =
    ProtoScalableMacro.protoableBuilderSetField[S, P, PF, SF](protoFieldSelector, value)

  inline def build: Protoable[S, P] = ProtoScalableMacro.buildProtoable[S, P]
}

object ProtoableBuilder {
  val _default = new ProtoableBuilder[Nothing, Nothing]
  inline def apply[S, P <: Message]: ProtoableBuilder[S, P] = ProtoScalableMacro.protoableBuilder[S, P]
}
