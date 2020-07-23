package pbconverts

case class Person(id: Long, name: String, phone: Option[String], hobbies: Seq[String])
