package pbconverts

import scala.collection.JavaConverters._
import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait ScalableImplicits {

  // java.lang.Iterable[M] => That[T]
  implicit def iterableScalable[That[_], T, M](implicit
      scalable: Scalable[T, M],
      bf: CanBuildFrom[Nothing, T, That[T]]
  ): Scalable[That[T], java.lang.Iterable[M]] =
    Scalable { proto ⇒
      proto.asScala.map(scalable.toScala).to[That]
    }

  implicit def iterableScalable2[That[_], T](implicit bf: CanBuildFrom[Nothing, T, That[T]]): Scalable[That[T], java.lang.Iterable[T]] =
    Scalable { proto ⇒ proto.asScala.to[That] }

  // java.lang.Iterable[M] => Array[T]
  implicit def arrayScalable[T: ClassTag, M](implicit scalable: Scalable[T, M]): Scalable[Array[T], java.lang.Iterable[M]] =
    Scalable { proto ⇒
      proto.asScala.map(scalable.toScala).toArray
    }

  implicit def arrayScalable2[T: ClassTag]: Scalable[Array[T], java.lang.Iterable[T]] = Scalable { proto ⇒ proto.asScala.toArray }

  // Repr[M] => That[T]
  implicit def iterableSelfScalable[That, Repr <: TraversableLike[M, Repr], T, M](implicit
      scalable: Scalable[T, M],
      bf: CanBuildFrom[Repr, T, That]
  ): Scalable[That, Repr] = { proto ⇒
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
