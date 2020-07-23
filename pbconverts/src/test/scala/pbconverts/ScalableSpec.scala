package pbconverts

import com.google.protobuf.StringValue
import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.PersonDto

class ScalableSpec extends AnyFunSuite {
  test("Scalable") {
    val person = Person(1L, "person name", Some("123456"), Seq("play games", "sing song"))

    val builder = PersonDto.newBuilder()
    builder.setId(1L)
    builder.setName("person name")
    builder.setPhone(StringValue.of("123456"))
    builder.addHobbies("play games")
    builder.addHobbies("sing song")
    val personDto = builder.build()

    val person1 = Person(
      personDto.getId,
      personDto.getName,
      if (personDto.hasPhone) Some(personDto.getPhone.getValue) else None,
      scala.collection.JavaConverters.iterableAsScalaIterable(personDto.getHobbiesList).toSeq
    )

    val person2 = Scalable[Person, PersonDto].toScala(builder.build())

    assert(person1 == person2)
  }
}
