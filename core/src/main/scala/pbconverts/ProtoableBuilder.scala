package pbconverts

import com.google.protobuf.Message

class ProtoableBuilder[T, M <: Message]() {

  def setField[TF, MF](protoFieldSelector: M ⇒ MF, value: T ⇒ TF): this.type = macro ProtoScalableMacro.setProtoFieldImpl[T, M, TF, MF]

  def setFieldValue[TF, MF](protoFieldSelector: M ⇒ MF, value: TF): this.type = macro ProtoScalableMacro.setProtoFieldImpl[T, M, TF, MF]

  def build: Protoable[T, M] = macro ProtoScalableMacro.buildProtoableImpl[T, M]
}

object ProtoableBuilder {
  def apply[T, M <: Message]: ProtoableBuilder[T, M] = macro ProtoScalableMacro.protoableBuilderApply[T, M]
}
