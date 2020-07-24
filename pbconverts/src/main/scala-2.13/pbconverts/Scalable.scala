package pbconverts

import java.lang
import java.time.{Instant, ZoneId, ZonedDateTime}

import com.google.protobuf.{BoolValue, DoubleValue, FloatValue, Int32Value, Int64Value, StringValue, Timestamp}

import scala.collection.BuildFrom
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

trait Scalable[+T, -M] {
  def toScala(proto: M): T
}

object Scalable {

  def apply[T <: Product, M]: Scalable[T, M] = macro ProtoScalableMacro.scalasImpl[T, M]

  def apply[T, M](convert: M ⇒ T): Scalable[T, M] =
    new Scalable[T, M] {
      override def toScala(proto: M): T = convert(proto)
    }

  implicit val javaIntegerScalable: Scalable[Int, Integer] = Scalable(_.toInt)
  implicit val javaLongScalable: Scalable[Long, lang.Long] = Scalable(_.toLong)
  implicit val javaDoubleScalable: Scalable[Double, lang.Double] = Scalable(_.toDouble)
  implicit val javaFloatScalable: Scalable[Float, lang.Float] = Scalable(_.toFloat)
  implicit val javaCharacterScalable: Scalable[Char, Character] = Scalable(_.toChar)
  implicit val javaByteScalable: Scalable[Byte, lang.Byte] = Scalable(_.toByte)

  implicit val stringValueScalable: Scalable[String, StringValue] = Scalable(_.getValue)
  implicit val int32ValueScalable: Scalable[Int, Int32Value] = Scalable(_.getValue)
  implicit val int64ValueScalable: Scalable[Long, Int64Value] = Scalable(_.getValue)
  implicit val doubleValueScalable: Scalable[Double, DoubleValue] = Scalable(_.getValue)
  implicit val floatValueScalable: Scalable[Float, FloatValue] = Scalable(_.getValue)
  implicit val boolValueScalable: Scalable[Boolean, BoolValue] = Scalable(_.getValue)

  implicit val zonedDateTimeProtoable: Scalable[ZonedDateTime, Timestamp] = Scalable { proto ⇒
    Instant.ofEpochSecond(proto.getSeconds, proto.getNanos).atZone(ZoneId.systemDefault())
  }

//   M => Option[T]
  implicit def optScalable[T, M](implicit scalable: Scalable[T, M]): Scalable[Option[T], M] =
    Scalable { proto ⇒
      Option(scalable.toScala(proto))
    }

  implicit def optScalable2[T]: Scalable[Option[T], T] = Scalable(Option(_))

//   java.lang.Iterable[M] => That[T]
  implicit def iterableScalable[That[_], T, M](implicit
      scalable: Scalable[T, M],
      bf: BuildFrom[Seq[_], T, That[T]]
  ): Scalable[That[T], java.lang.Iterable[M]] =
    Scalable { proto ⇒
      bf.fromSpecific(Seq.empty)(proto.asScala.iterator.map(scalable.toScala))
    }

  implicit def iterableScalable2[That[T], T](implicit bf: BuildFrom[Seq[T], T, That[T]]): Scalable[That[T], java.lang.Iterable[T]] =
    Scalable { proto ⇒
      bf.fromSpecific(Seq.empty)(proto.asScala)
    }

//   java.lang.Iterable[M] => Array[T]
  implicit def arrayScalable[T: ClassTag, M](implicit scalable: Scalable[T, M]): Scalable[Array[T], java.lang.Iterable[M]] =
    Scalable { proto ⇒
      proto.asScala.map(scalable.toScala).toArray
    }

  implicit def arrayScalable2[T: ClassTag]: Scalable[Array[T], java.lang.Iterable[T]] = Scalable { proto ⇒ proto.asScala.toArray }

//  // Repr[M] => That[T]
  implicit def iterableSelfScalable[That, Repr <: Iterable[M], T, M](implicit
      scalable: Scalable[T, M],
      bf: BuildFrom[Iterable[_], T, That]
  ): Scalable[That, Repr] = { proto ⇒
    bf.newBuilder(proto.map(scalable.toScala)).result()
  }

  implicit def mapScalable4[K1, K2, V1, V2](implicit kScalable: Scalable[K1, K2], vScalable: Scalable[V1, V2]): Scalable[Map[K1, V1], java.util.Map[K2, V2]] =
    Scalable[Map[K1, V1], java.util.Map[K2, V2]] { m ⇒
      m.asScala.map { case (k, v) ⇒ kScalable.toScala(k) -> vScalable.toScala(v) }.toMap
    }

  implicit def mapScalable3[K, V1, V2](implicit vScalable: Scalable[V1, V2]): Scalable[Map[K, V1], java.util.Map[K, V2]] =
    Scalable[Map[K, V1], java.util.Map[K, V2]] { m ⇒ m.asScala.map { case (k, v) ⇒ k -> vScalable.toScala(v) }.toMap }

  implicit def mapScalable2[K1, K2, V](implicit kScalable: Scalable[K1, K2]): Scalable[Map[K1, V], java.util.Map[K2, V]] =
    Scalable[Map[K1, V], java.util.Map[K2, V]] { m ⇒ m.asScala.map { case (k, v) ⇒ kScalable.toScala(k) -> v }.toMap }

  implicit def mapScalable1[K, V]: Scalable[Map[K, V], java.util.Map[K, V]] =
    Scalable[Map[K, V], java.util.Map[K, V]] { m ⇒ m.asScala.toMap }

}
