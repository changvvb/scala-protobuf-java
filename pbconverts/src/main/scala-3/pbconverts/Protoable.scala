package pbconverts

import annotation.targetName
import java.time.ZonedDateTime
import com.google.protobuf.{BoolValue, DoubleValue, FloatValue, GeneratedMessageV3, Int32Value, Int64Value, Message, MessageOrBuilder, StringValue, Timestamp}

import scala.collection.JavaConverters._

trait Protoable[-S, +P] {
  def toProto(entity: S): P
}

object Protoable {

  inline def apply[S <: Product, P <: GeneratedMessageV3 with MessageOrBuilder]: Protoable[S, P] =
    ProtoScalableMacro.protoable[S, P]

  def apply[S, P](f: S ⇒ P): Protoable[S, P] =
    new Protoable[S, P] {
      override def toProto(entity: S): P = f(entity)
    }

  given Protoable[Double, java.lang.Double] = Protoable(_.toDouble)
  given Protoable[Float, java.lang.Float] = Protoable(_.toFloat)
  given Protoable[Int, java.lang.Integer] = Protoable(_.toInt)
  given Protoable[Long, java.lang.Long] = Protoable(_.toLong)
  given Protoable[Char, java.lang.Character] = Protoable(_.toChar)
  given Protoable[Byte, java.lang.Byte] = Protoable(_.toByte)

  given Protoable[String, StringValue] = Protoable(StringValue.of)
  given Protoable[Double, DoubleValue] = Protoable(DoubleValue.of)
  given Protoable[Float, FloatValue] = Protoable(FloatValue.of)
  given Protoable[Int, Int32Value] = Protoable(Int32Value.of)
  given Protoable[Boolean, BoolValue] = Protoable(BoolValue.of)
  given Protoable[Long, Int64Value] = Protoable(Int64Value.of)

  given Protoable[ZonedDateTime, Timestamp] = Protoable { entity ⇒
    Timestamp.newBuilder().setSeconds(entity.toEpochSecond).setNanos(entity.getNano).build()
  }

  given [T, M](using protoable: Protoable[T, M]): Protoable[scala.Iterable[T], java.util.List[M]] =
    Protoable[scala.Iterable[T], java.util.List[M]] { entity ⇒
      entity.toList.map(protoable.toProto).asJava
    }

  given [T]: Protoable[scala.Iterable[T], java.util.List[T]] =
    Protoable[scala.Iterable[T], java.util.List[T]] { entity ⇒ entity.toList.asJava }

  given [T]: Protoable[Array[T], java.util.List[T]] =
    Protoable[Array[T], java.util.List[T]] { entity ⇒ entity.toList.asJava }

  given [T, M](using protoable: Protoable[T, M]): Protoable[Array[T], java.lang.Iterable[M]] =
    Protoable[Array[T], java.util.List[M]] { entity ⇒
      entity.toList.map(protoable.toProto).asJava
    }

  given [F, Target <: Any](using protoable: Protoable[F, Target]): Protoable[Option[F], Target] =
    Protoable[Option[F], Target] { (entity: Option[F]) ⇒
      entity.map(protoable.toProto).getOrElse(None.orNull.asInstanceOf[Target])
    }

  given [T]: Protoable[Option[T], T] =
    Protoable[Option[T], T] { (opt: Option[T]) => opt.getOrElse(None.orNull.asInstanceOf[T]) }

  given [K, V]: Protoable[Map[K, V], java.util.Map[K, V]] =
    Protoable[Map[K, V], java.util.Map[K, V]] { m ⇒ m.asJava }

  @targetName("for key mapping")
  given [K1, K2, V](using kProtoable: Protoable[K1, K2]): Protoable[Map[K1, V], java.util.Map[K2, V]] =
    Protoable[Map[K1, V], java.util.Map[K2, V]] { m ⇒ m.map { case (k, v) ⇒ kProtoable.toProto(k) -> v }.asJava }

  given [K, V1, V2](using vProtoable: Protoable[V1, V2]): Protoable[Map[K, V1], java.util.Map[K, V2]] =
    Protoable[Map[K, V1], java.util.Map[K, V2]] { m ⇒ m.map { case (k, v) ⇒ k -> vProtoable.toProto(v) }.asJava }

  given [K1, K2, V1, V2](using
      kProtoable: Protoable[K1, K2],
      vProtoable: Protoable[V1, V2]
  ): Protoable[Map[K1, V1], java.util.Map[K2, V2]] =
    Protoable[Map[K1, V1], java.util.Map[K2, V2]] { m ⇒
      m.map { case (k, v) ⇒ kProtoable.toProto(k) -> vProtoable.toProto(v) }.asJava
    }
}
