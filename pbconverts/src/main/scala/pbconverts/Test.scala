package pbconverts

object Test {

  given Int = 1
  implicitly[Int].toString
}
