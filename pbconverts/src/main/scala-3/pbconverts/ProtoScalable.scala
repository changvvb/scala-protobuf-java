package pbconverts

import com.google.protobuf.Message

trait ProtoScalable[T, M] extends Protoable[T, M] with Scalable[T, M]

object ProtoScalable {

  inline def apply[T <: Product, M <: Message]: ProtoScalable[T, M] = ProtoScalableMacro.protoScalable[T, M]

  def apply[T, M](toProtoFun: T ⇒ M, toScalaFun: M ⇒ T): ProtoScalable[T, M] =
    new ProtoScalable[T, M] {

      override def toScala(proto: M): T = toScalaFun(proto)

      override def toProto(entity: T): M = toProtoFun(entity)
    }
}
