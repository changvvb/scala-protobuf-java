package pbconverts

import scala.collection.BuildFrom
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

trait ScalableImplicits {

  //   java.lang.Iterable[M] => That[T]
  implicit def iterableScalable[That[_], S, P](implicit
      scalable: Scalable[S, P],
      bf: BuildFrom[Seq[_], S, That[S]]
  ): Scalable[That[S], java.lang.Iterable[P]] =
    Scalable { proto ⇒
      bf.fromSpecific(Seq.empty)(proto.asScala.iterator.map(scalable.toScala))
    }

  implicit def iterableScalable2[That[T], T](implicit bf: BuildFrom[Seq[T], T, That[T]]): Scalable[That[T], java.lang.Iterable[T]] =
    Scalable { proto ⇒
      bf.fromSpecific(Seq.empty)(proto.asScala)
    }

  //  // Repr[M] => That[T]
  implicit def iterableSelfScalable[That, Repr <: Iterable[P], S, P](implicit
      scalable: Scalable[S, P],
      bf: BuildFrom[Iterable[_], S, That]
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
