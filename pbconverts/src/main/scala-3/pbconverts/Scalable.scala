package pbconverts

import java.time.{Instant, ZoneId, ZonedDateTime}
import com.google.protobuf.{BoolValue, DoubleValue, FloatValue, Int32Value, Int64Value, StringValue, Timestamp}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag
import com.google.protobuf.Message

trait Scalable[+T, -M] {
  def toScala(proto: M): T
}

object Scalable extends ScalableImplicits {

  inline def apply[T <: Product, M <: Message]: Scalable[T, M] = ProtoScalableMacro.scalable[T, M]

  def apply[T, M](convert: M ⇒ T): Scalable[T, M] =
    new Scalable[T, M] {
      override def toScala(proto: M): T = convert(proto)
    }

  given Int = 1

  given Scalable[Int, java.lang.Integer] = Scalable(_.toInt)
  given Scalable[Long, java.lang.Long] = Scalable(_.toLong)

  given Scalable[Double, java.lang.Double] = Scalable(_.toDouble)
  given Scalable[Float, java.lang.Float] = Scalable(_.toFloat)
  given Scalable[Char, java.lang.Character] = Scalable(_.toChar)
  given javaByteScalable: Scalable[Byte, java.lang.Byte] = Scalable(_.toByte)

  given Scalable[String, StringValue] = Scalable(_.getValue)
  given Scalable[Int, Int32Value] = Scalable(_.getValue)
  given Scalable[Long, Int64Value] = Scalable(_.getValue)
  given Scalable[Double, DoubleValue] = Scalable(_.getValue)
  given Scalable[Float, FloatValue] = Scalable(_.getValue)
  given Scalable[Boolean, BoolValue] = Scalable(_.getValue)

  given Scalable[ZonedDateTime, Timestamp] = Scalable { proto ⇒
    Instant.ofEpochSecond(proto.getSeconds, proto.getNanos).atZone(ZoneId.systemDefault())
  }

  // java.lang.Iterable[M] => Array[T]
  given [T: ClassTag, M](using scalable: Scalable[T, M]): Scalable[Array[T], java.lang.Iterable[M]] =
    Scalable { proto ⇒
      proto.asScala.map(scalable.toScala).toArray
    }

  given [T: ClassTag]: Scalable[Array[T], java.lang.Iterable[T]] = Scalable { proto ⇒ proto.asScala.toArray }

  // M => Option[T]
  given [T, M](using scalable: Scalable[T, M]): Scalable[Option[T], M] =
    Scalable { proto ⇒
      Option(scalable.toScala(proto))
    }

  given [T]: Scalable[Option[T], T] = Scalable(Option(_))

}
