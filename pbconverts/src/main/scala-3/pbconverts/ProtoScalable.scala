package pbconverts

import com.google.protobuf.Message

trait ProtoScalable[S, P] extends Protoable[S, P] with Scalable[S, P]

object ProtoScalable {

  inline def apply[S <: Product, P <: Message]: ProtoScalable[S, P] = ProtoScalableMacro.protoScalable[S, P]

  def apply[S, P](toProtoFun: S ⇒ P, toScalaFun: P ⇒ S): ProtoScalable[S, P] =
    new ProtoScalable[S, P] {

      override def toScala(proto: P): S = toScalaFun(proto)

      override def toProto(entity: S): P = toProtoFun(entity)
    }
}
