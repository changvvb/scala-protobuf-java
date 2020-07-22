package com.github.changvvb.conversion

import java.time.ZonedDateTime

import com.google.protobuf.{BoolValue, DoubleValue, FloatValue, Int32Value, Int64Value, StringValue, Timestamp}

import scala.collection.JavaConverters._

trait Protoable[-T, +M] {
  def toProto(entity: T): M
}

object Protoable {

  def apply[T <: Product, M]: Protoable[T, M] = macro ProtoScalableMacro.protosImpl[T, M]

  def apply[T, M](f: T ⇒ M): Protoable[T, M] = new Protoable[T, M] {
    override def toProto(entity: T): M = f(entity)
  }

  implicit val javaDoubleProtoable = Protoable[Double, java.lang.Double](_.toDouble)
  implicit val javaFloatProtoable = Protoable[Float, java.lang.Float](_.toFloat)
  implicit val javaIntergerProtoable = Protoable[Int, java.lang.Integer](_.toInt)
  implicit val javaLongProtoable = Protoable[Long, java.lang.Long](_.toLong)
  implicit val javaCharacterProtoable = Protoable[Char, java.lang.Character](_.toChar)
  implicit val javaByteProtoable = Protoable[Byte, java.lang.Byte](_.toByte)

  implicit val stringValueProtoable = Protoable[String, StringValue](StringValue.of)
  implicit val doubleValueProtoable = Protoable[Double, DoubleValue](DoubleValue.of)
  implicit val floatValueProtoable = Protoable[Float, FloatValue](FloatValue.of)
  implicit val int32ValueProtoable = Protoable[Int, Int32Value](Int32Value.of)
  implicit val boolValueProtoable = Protoable[Boolean, BoolValue](BoolValue.of)
  implicit val int64ValueProtoable = Protoable[Long, Int64Value](Int64Value.of)

  implicit val zonedDateTimeProtoable = Protoable[ZonedDateTime, Timestamp] { entity ⇒
    Timestamp.newBuilder().setSeconds(entity.toEpochSecond).setNanos(entity.getNano).build()
  }

  implicit def iterableProtoable[T, M](implicit protoable: Protoable[T, M]): Protoable[scala.Iterable[T], java.util.List[M]] =
    Protoable[scala.Iterable[T], java.util.List[M]] { entity ⇒
      entity.toList.map(protoable.toProto).asJava
    }

  implicit def iterableProtoable2[T]: Protoable[scala.Iterable[T], java.util.List[T]] =
    Protoable[scala.Iterable[T], java.util.List[T]] { entity ⇒ entity.toList.asJava }

  implicit def arrayProtoable[T, M](implicit protoable: Protoable[T, M]): Protoable[Array[T], java.util.List[M]] =
    Protoable[Array[T], java.util.List[M]] { entity ⇒
      entity.toList.map(protoable.toProto).asJava
    }

  implicit def arrayProtoable[T]: Protoable[Array[T], java.util.List[T]] =
    Protoable[Array[T], java.util.List[T]] { entity ⇒ entity.toList.asJava }

  implicit def optProtoable[F, Target <: Any](implicit protoable: Protoable[F, Target]): Protoable[Option[F], Target] =
    Protoable[Option[F], Target] { entity:Option[F] ⇒
      entity.map(protoable.toProto).getOrElse(None.orNull.asInstanceOf[Target])
    }

  implicit def optProtoable2[T]: Protoable[Option[T], T] =
    Protoable[Option[T], T] { opt:Option[T] => opt.getOrElse(None.orNull.asInstanceOf[T]) }

  implicit def mapProtoable1[K, V]: Protoable[Map[K, V], java.util.Map[K, V]] =
    Protoable[Map[K, V], java.util.Map[K, V]] { m ⇒ m.asJava }

  implicit def mapProtoable2[K1, K2, V](implicit kProtoable: Protoable[K1, K2]): Protoable[Map[K1, V], java.util.Map[K2, V]] =
    Protoable[Map[K1, V], java.util.Map[K2, V]] { m ⇒ m.map { case (k, v) ⇒ kProtoable.toProto(k) -> v }.asJava }

  implicit def mapProtoable3[K, V1, V2](implicit vProtoable: Protoable[V1, V2]): Protoable[Map[K, V1], java.util.Map[K, V2]] =
    Protoable[Map[K, V1], java.util.Map[K, V2]] { m ⇒ m.map { case (k, v) ⇒ k -> vProtoable.toProto(v) }.asJava }

  implicit def mapProtoable4[K1, K2, V1, V2]
    (implicit kProtoable: Protoable[K1, K2], vProtoable: Protoable[V1, V2]): Protoable[Map[K1, V1], java.util.Map[K2, V2]] =
    Protoable[Map[K1, V1], java.util.Map[K2, V2]] { m ⇒
      m.map { case (k, v) ⇒ kProtoable.toProto(k) -> vProtoable.toProto(v) }.asJava
    }
}
