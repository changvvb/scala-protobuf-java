package pbconverts

object Converter {

  def toProto[S, P](d: S)(implicit protoable: Protoable[S, P]): P = protoable.toProto(d)

  def toScala[S, P](m: P)(implicit scalable: Scalable[S, P]): S = scalable.toScala(m)

}
