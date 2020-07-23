package pbconverts

import com.google.protobuf.StringValue
import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.PersonDto

class ProtoableSpec extends AnyFunSuite {
  test("Protoable") {
    val person = Person(1L, "person name", Some("123456"), Seq("play games", "sing song"))

    val builder = PersonDto.newBuilder()
    builder.setId(person.id)
    builder.setName(person.name)
    person.phone.foreach(p => builder.setPhone(StringValue.of(p)))
    person.hobbies.foreach(builder.addHobbies)
    val personDto: PersonDto = builder.build()

    val personDto2: PersonDto = Protoable[Person, PersonDto].toProto(person)
    assert(personDto == personDto2)
  }
}
