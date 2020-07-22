package com.github.changvvb.conversion

trait ProtoScalable[T, M] extends Protoable[T, M] with Scalable[T, M]

object ProtoScalable {

  def apply[T <: Product, M]: ProtoScalable[T, M] = macro ProtoScalableMacro.convertsImpl[T, M]

  def apply[T, M](toProtoFun: T ⇒ M, toScalaFun: M ⇒ T): ProtoScalable[T, M] = new ProtoScalable[T, M] {

    override def toScala(proto: M): T = toScalaFun(proto)

    override def toProto(entity: T): M = toProtoFun(entity)
  }
}
