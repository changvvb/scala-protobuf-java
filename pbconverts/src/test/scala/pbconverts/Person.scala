package pbconverts

import pbconverts.ConversionTest.PBPerson

case class Person(id: Long, name: String, phone: Option[String], hobbies: Seq[String])

object Person {
  def default = Person(1L, "name", Some("123"), hobbies = Seq("ping pong"))

  implicit val protoScalable: ProtoScalable[Person, PBPerson] = ProtoScalable[Person, PBPerson]
}
