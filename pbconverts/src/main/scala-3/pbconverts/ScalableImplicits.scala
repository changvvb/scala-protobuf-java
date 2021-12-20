package pbconverts

import annotation.targetName

import scala.collection.BuildFrom
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

trait ScalableImplicits {

//     java.lang.Iterable[M] => That[T]
  given [That[_], T, M](using scalable: Scalable[T, M], bf: BuildFrom[Seq[_], T, That[T]]): Scalable[That[T], java.lang.Iterable[M]] =
    Scalable { proto ⇒
      bf.fromSpecific(Seq.empty)(proto.asScala.iterator.map(scalable.toScala))
    }

  given [That[T], T](using bf: BuildFrom[Seq[T], T, That[T]]): Scalable[That[T], java.lang.Iterable[T]] =
    Scalable { proto ⇒
      bf.fromSpecific(Seq.empty)(proto.asScala)
    }

  //  // Repr[M] => That[T]
  given [That, Repr <: Iterable[M], T, M](using
      scalable: Scalable[T, M],
      bf: BuildFrom[Iterable[_], T, That]
  ): Scalable[That, Repr] = { proto ⇒
    bf.newBuilder(proto.map(scalable.toScala)).result()
  }

  given [K1, K2, V1, V2](using kScalable: Scalable[K1, K2], vScalable: Scalable[V1, V2]): Scalable[Map[K1, V1], java.util.Map[K2, V2]] =
    Scalable[Map[K1, V1], java.util.Map[K2, V2]] { m ⇒
      m.asScala.map { case (k, v) ⇒ kScalable.toScala(k) -> vScalable.toScala(v) }.toMap
    }

  given [K, V1, V2](using vScalable: Scalable[V1, V2]): Scalable[Map[K, V1], java.util.Map[K, V2]] =
    Scalable[Map[K, V1], java.util.Map[K, V2]] { m ⇒ m.asScala.map { case (k, v) ⇒ k -> vScalable.toScala(v) }.toMap }

  @targetName("key mapping")
  given [K1, K2, V](using kScalable: Scalable[K1, K2]): Scalable[Map[K1, V], java.util.Map[K2, V]] =
    Scalable[Map[K1, V], java.util.Map[K2, V]] { m ⇒ m.asScala.map { case (k, v) ⇒ kScalable.toScala(k) -> v }.toMap }

  given [K, V]: Scalable[Map[K, V], java.util.Map[K, V]] =
    Scalable[Map[K, V], java.util.Map[K, V]] { m ⇒ m.asScala.toMap }
}
