package pbconverts

object Converter {

  def toProto[T, M](d: T)(using protoable: Protoable[T, M]): M = protoable.toProto(d)

  def toScala[T, M](m: M)(using scalable: Scalable[T, M]): T = scalable.toScala(m)

}
