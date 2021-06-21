package pbconverts

trait ProtoScalable[S, P] extends Protoable[S, P] with Scalable[S, P]

object ProtoScalable {

  def apply[S <: Product, P]: ProtoScalable[S, P] = macro ProtoScalableMacro.convertsImpl[S, P]

  def apply[S, P](toProtoFun: S ⇒ P, toScalaFun: P ⇒ S): ProtoScalable[S, P] =
    new ProtoScalable[S, P] {

      override def toScala(proto: P): S = toScalaFun(proto)

      override def toProto(entity: S): P = toProtoFun(entity)
    }
}
