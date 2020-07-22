package com.github.changvvb.conversion

object Converter {

  def toProto[T, M](d: T)(implicit protoable: Protoable[T, M]): M = protoable.toProto(d)

  def toScala[T, M](m: M)(implicit scalable: Scalable[T, M]): T = scalable.toScala(m)

}
