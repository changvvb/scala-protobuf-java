package pbconverts

import java.time.{Instant, ZoneId, ZonedDateTime}

import com.google.protobuf.{BoolValue, DoubleValue, FloatValue, Int32Value, Int64Value, StringValue, Timestamp}

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

trait Scalable[+T, -M] {
  def toScala(proto: M): T
}

object Scalable extends ScalableImplicits {

  def apply[T <: Product, M]: Scalable[T, M] = ??? // macro ProtoScalableMacro.scalasImpl[T, M]

  def apply[T, M](convert: M ⇒ T): Scalable[T, M] =
    new Scalable[T, M] {
      override def toScala(proto: M): T = convert(proto)
    }

  given Int = 1

  given Scalable[Int, java.lang.Integer] = Scalable[Int, java.lang.Integer].apply(_.toInt)
//  given javaLongScalable: Scalable[Long, java.lang.Long] with Scalable[Long, java.lang.Long](_.toLong)
//  given javaDoubleScalable:Scalable[Double, java.lang.Double] with Scalable[Double, java.lang.Double](_.toDouble)
//  given javaFloatScalable:Scalable[Float, java.lang.Float] with Scalable[Float, java.lang.Float](_.toFloat)
//  given javaCharacterScalable:Scalable[Char, java.lang.Character] with Scalable[Char, java.lang.Character](_.toChar)
//  given javaByteScalable:Scalable[Byte, java.lang.Byte] with Scalable[Byte, java.lang.Byte](_.toByte)

//  given Scalable [String, StringValue] = Scalable [String, StringValue] (_.getValue)
//  given Scalable [Int, Int32Value] = Scalable [Int, Int32Value] (_.getValue)
//  given Scalable [Long, Int64Value] = Scalable [Long, Int64Value] (_.getValue)
//  given Scalable [Double, DoubleValue] = Scalable [Double, DoubleValue] (_.getValue)
//  given  Scalable [Float, FloatValue] = Scalable [Float, FloatValue] (_.getValue)
//  given Scalable [Boolean, BoolValue] = Scalable [Boolean, BoolValue] (_.getValue)

  implicit val zonedDateTimeProtoable: Scalable[ZonedDateTime, Timestamp] = Scalable[ZonedDateTime, Timestamp] { proto ⇒
    Instant.ofEpochSecond(proto.getSeconds, proto.getNanos).atZone(ZoneId.systemDefault())
  }

  //   java.lang.Iterable[M] => Array[T]
  implicit def arrayScalable[T: ClassTag, M](implicit scalable: Scalable[T, M]): Scalable[Array[T], java.lang.Iterable[M]] =
    Scalable { proto ⇒
      proto.asScala.map(scalable.toScala).toArray
    }

  implicit def arrayScalable2[T: ClassTag]: Scalable[Array[T], java.lang.Iterable[T]] = Scalable { proto ⇒ proto.asScala.toArray }

  // M => Option[T]
  implicit def optScalable[T, M](implicit scalable: Scalable[T, M]): Scalable[Option[T], M] =
    Scalable { proto ⇒
      Option(scalable.toScala(proto))
    }

  implicit def optScalable2[T]: Scalable[Option[T], T] = Scalable(Option(_))

}
