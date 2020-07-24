package pbconverts

import java.time.{Instant, ZoneId, ZonedDateTime}

import com.google.protobuf.{BoolValue, DoubleValue, FloatValue, Int32Value, Int64Value, StringValue, Timestamp}

trait Scalable[+T, -M] {
  def toScala(proto: M): T
}

object Scalable extends ScalableImplicits {

  def apply[T <: Product, M]: Scalable[T, M] = macro ProtoScalableMacro.scalasImpl[T, M]

  def apply[T, M](convert: M ⇒ T): Scalable[T, M] =
    new Scalable[T, M] {
      override def toScala(proto: M): T = convert(proto)
    }

  implicit val javaIntegerScalable = Scalable[Int, java.lang.Integer](_.toInt)
  implicit val javaLongScalable = Scalable[Long, java.lang.Long](_.toLong)
  implicit val javaDoubleScalable = Scalable[Double, java.lang.Double](_.toDouble)
  implicit val javaFloatScalable = Scalable[Float, java.lang.Float](_.toFloat)
  implicit val javaCharacterScalable = Scalable[Char, java.lang.Character](_.toChar)
  implicit val javaByteScalable = Scalable[Byte, java.lang.Byte](_.toByte)

  implicit val stringValueScalable = Scalable[String, StringValue](_.getValue)
  implicit val int32ValueScalable = Scalable[Int, Int32Value](_.getValue)
  implicit val int64ValueScalable = Scalable[Long, Int64Value](_.getValue)
  implicit val doubleValueScalable = Scalable[Double, DoubleValue](_.getValue)
  implicit val floatValueScalable = Scalable[Float, FloatValue](_.getValue)
  implicit val boolValueScalable = Scalable[Boolean, BoolValue](_.getValue)

  implicit val zonedDateTimeProtoable = Scalable[ZonedDateTime, Timestamp] { proto ⇒
    Instant.ofEpochSecond(proto.getSeconds, proto.getNanos).atZone(ZoneId.systemDefault())
  }

  // M => Option[T]
  implicit def optScalable[T, M](implicit scalable: Scalable[T, M]): Scalable[Option[T], M] =
    Scalable { proto ⇒
      Option(scalable.toScala(proto))
    }

  implicit def optScalable2[T]: Scalable[Option[T], T] = Scalable(Option(_))

}
