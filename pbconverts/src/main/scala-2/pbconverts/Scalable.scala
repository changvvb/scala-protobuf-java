package pbconverts

import java.time.{Instant, ZoneId, ZonedDateTime}

import com.google.protobuf.{BoolValue, DoubleValue, FloatValue, Int32Value, Int64Value, StringValue, Timestamp}

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

trait Scalable[+S, -P] {
  def toScala(proto: P): S
}

object Scalable extends ScalableImplicits {

  def apply[S <: Product, P]: Scalable[S, P] = macro ProtoScalableMacro.scalasImpl[S, P]

  def apply[S, P](convert: P ⇒ S): Scalable[S, P] = x => convert(x)

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

  //   java.lang.Iterable[M] => Array[T]
  implicit def arrayScalable[S: ClassTag, P](implicit scalable: Scalable[S, P]): Scalable[Array[S], java.lang.Iterable[P]] =
    Scalable { proto ⇒
      proto.asScala.map(scalable.toScala).toArray
    }

  implicit def arrayScalable2[T: ClassTag]: Scalable[Array[T], java.lang.Iterable[T]] = Scalable { proto ⇒ proto.asScala.toArray }

  // M => Option[T]
  implicit def optScalable[S, P](implicit scalable: Scalable[S, P]): Scalable[Option[S], P] =
    Scalable { proto ⇒
      Option(scalable.toScala(proto))
    }

  implicit def optScalable2[T]: Scalable[Option[T], T] = Scalable(Option(_))

}
