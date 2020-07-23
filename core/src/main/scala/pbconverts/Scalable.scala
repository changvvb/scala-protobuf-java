package pbconverts

import java.time.{Instant, ZoneId, ZonedDateTime}

import com.google.protobuf.{BoolValue, DoubleValue, FloatValue, Int32Value, Int64Value, StringValue, Timestamp}

import scala.collection.JavaConverters._
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait Scalable[+T, -M] {
  def toScala(proto: M): T
}

object Scalable {

  def apply[T <: Product, M]: Scalable[T, M] = macro ProtoScalableMacro.scalasImpl[T, M]

  def apply[T, M](convert: M ⇒ T): Scalable[T, M] = new Scalable[T, M] {
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
  implicit def optScalable[T, M](implicit scalable: Scalable[T, M]): Scalable[Option[T], M] = Scalable { proto ⇒
    Option(scalable.toScala(proto))
  }

  implicit def optScalable2[T]: Scalable[Option[T], T] = Scalable(Option(_))

  // java.lang.Iterable[M] => That[T]
  implicit def iterableScalable[That[_], T, M]
    (implicit scalable: Scalable[T, M], bf: CanBuildFrom[Nothing, T, That[T]]): Scalable[That[T], java.lang.Iterable[M]] = Scalable { proto ⇒
    proto.asScala.map(scalable.toScala).to[That]
  }

  implicit def iterableScalable2[That[_], T](implicit bf: CanBuildFrom[Nothing, T, That[T]]): Scalable[That[T], java.lang.Iterable[T]] =
    Scalable { proto ⇒ proto.asScala.to[That] }

  // java.lang.Iterable[M] => Array[T]
  implicit def arrayScalable[T: ClassTag, M](implicit scalable: Scalable[T, M]): Scalable[Array[T], java.lang.Iterable[M]] = Scalable { proto ⇒
    proto.asScala.map(scalable.toScala).toArray
  }

  implicit def arrayScalable2[T: ClassTag]: Scalable[Array[T], java.lang.Iterable[T]] = Scalable { proto ⇒ proto.asScala.toArray }

  // Repr[M] => That[T]
  implicit def iterableSelfScalable[That, Repr <: TraversableLike[M, Repr], T, M]
    (implicit scalable: Scalable[T, M], bf: CanBuildFrom[Repr, T, That]): Scalable[That, Repr] = { proto ⇒
    proto.map(scalable.toScala)
  }

  implicit def mapScalable1[K, V]: Scalable[Map[K, V], java.util.Map[K, V]] =
    Scalable[Map[K, V], java.util.Map[K, V]] { m ⇒ m.asScala.toMap }

  implicit def mapScalable2[K1, K2, V](implicit kScalable: Scalable[K1, K2]): Scalable[Map[K1, V], java.util.Map[K2, V]] =
    Scalable[Map[K1, V], java.util.Map[K2, V]] { m ⇒ m.asScala.map { case (k, v) ⇒ kScalable.toScala(k) -> v }.toMap }

  implicit def mapScalable3[K, V1, V2](implicit vScalable: Scalable[V1, V2]): Scalable[Map[K, V1], java.util.Map[K, V2]] =
    Scalable[Map[K, V1], java.util.Map[K, V2]] { m ⇒ m.asScala.map { case (k, v) ⇒ k -> vScalable.toScala(v) }.toMap }

  implicit def mapScalable4[K1, K2, V1, V2](implicit kScalable: Scalable[K1, K2], vScalable: Scalable[V1, V2]): Scalable[Map[K1, V1], java.util.Map[K2, V2]] =
    Scalable[Map[K1, V1], java.util.Map[K2, V2]] { m ⇒
      m.asScala.map { case (k, v) ⇒ kScalable.toScala(k) -> vScalable.toScala(v) }.toMap
    }
}
