package pbconverts

import com.google.protobuf.StringValue
import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.PBPerson

class ProtoableSpec extends AnyFunSuite {
  test("Protoable") {
    val person = Person(1L, "person name", Some("123456"), Seq("play games", "sing song"))

    val builder = PBPerson.newBuilder()
    builder.setId(person.id)
    builder.setName(person.name)
    person.phone.foreach(p => builder.setPhone(StringValue.of(p)))
    person.hobbies.foreach(builder.addHobbies)
    val pbPerson: PBPerson = builder.build()

    val pbPerson2: PBPerson = Protoable[Person, PBPerson].toProto(person)
    assert(pbPerson == pbPerson2)
  }
}
