package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.PBTestMessage
import pbconverts.ConversionTest.PBPerson

class ProtoableBuilderSpec extends AnyFunSuite {
  test("test ProtoableBuilder") {
    assertTestMessage(TestMessage.default)
    assertTestMessage(TestMessage.zero)
  }

  test("setFieldValue") {
    val person = Person(1, "name", Some("123"), Seq("play"))
    val pbPerson = ProtoableBuilder[Person, PBPerson]
      .setFieldValue(_.getName, "my name")
      .build
      .toProto(person)

    assert(pbPerson.getName == "my name")
    assert(pbPerson.getId == 1)
  }

  def assertTestMessage(testMessage: TestMessage) = {
    val pbTestMessage: PBTestMessage = ProtoableBuilder[TestMessage, PBTestMessage]
      .setField(_.getIntValue, _.intValue + 1)
      .build
      .toProto(testMessage)

    assert(pbTestMessage.getIntValue == testMessage.intValue + 1)
    val testMessage2 = Scalable[TestMessage, PBTestMessage].toScala(pbTestMessage)

    assert(testMessage2.intArray.sameElements(testMessage.intArray))
    assert(testMessage2.stringArray.sameElements(testMessage.stringArray))
    assert(testMessage2.personArray.sameElements(testMessage.personArray))

    assert(
      testMessage == testMessage2.copy(
        intValue = testMessage2.intValue - 1,
        intArray = testMessage.intArray,
        stringArray = testMessage.stringArray,
        personArray = testMessage.personArray
      )
    )
  }
}
