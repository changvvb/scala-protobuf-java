package pbconverts

import com.google.protobuf.Message

class ProtoableBuilder[S, P <: Message]() {

  def setField[SF, PF](protoFieldSelector: P ⇒ PF, value: S ⇒ SF): this.type = macro ProtoScalableMacro.setProtoFieldImpl[S, P, SF, PF]

  def setFieldValue[SF, PF](protoFieldSelector: P ⇒ PF, value: SF): this.type = macro ProtoScalableMacro.setProtoFieldImpl[S, P, SF, PF]

  def build: Protoable[S, P] = macro ProtoScalableMacro.buildProtoableImpl[S, P]
}

object ProtoableBuilder {
  def apply[S, P <: Message]: ProtoableBuilder[S, P] = macro ProtoScalableMacro.protoableBuilderApply[S, P]
}
