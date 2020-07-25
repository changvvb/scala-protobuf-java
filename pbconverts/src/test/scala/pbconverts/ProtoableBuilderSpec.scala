package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.TestMessageDto

class ProtoableBuilderSpec extends AnyFunSuite {
  test("test ProtoableBuilder") {
    assertTestMessage(TestMessage.default)
    assertTestMessage(TestMessage.zero)
  }

  def assertTestMessage(testMessage: TestMessage) = {
    val testMessage = TestMessage.default
    val testMessageDto: TestMessageDto = ProtoableBuilder[TestMessage, TestMessageDto]
      .setField(_.getIntValue, _.intValue + 1)
      .build
      .toProto(testMessage)

    assert(testMessageDto.getIntValue == testMessage.intValue + 1)
    val testMessage2 = Scalable[TestMessage, TestMessageDto].toScala(testMessageDto)

    assert(testMessage2.intArray.sameElements(testMessage.intArray))
    assert(testMessage == testMessage2.copy(intValue = testMessage2.intValue - 1, intArray = testMessage.intArray, stringArray = testMessage.stringArray))
  }
}
