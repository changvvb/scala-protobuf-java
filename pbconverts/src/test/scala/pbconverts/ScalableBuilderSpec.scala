package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.TestMessageDto

class ScalableBuilderSpec extends AnyFunSuite {
  test("test ScalableBuilder") {
    assertTestMessage(TestMessage.default)
    assertTestMessage(TestMessage.zero)
  }

  def assertTestMessage(testMessage: TestMessage) = {
    val testMessageDto = Protoable[TestMessage, TestMessageDto].toProto(testMessage)
    val testMessage2 = ScalableBuilder[TestMessage, TestMessageDto]
      .setField(_.intOpt, m => if (m.hasIntOpt) Some(m.getIntOpt.getValue + 1) else None)
      .build
      .toScala(testMessageDto)

    assert(testMessage2.intArray.sameElements(testMessage.intArray))
    assert(testMessage2.stringArray.sameElements(testMessage.stringArray))
    assert(testMessage == testMessage2.copy(intOpt = testMessage2.intOpt.map(_ - 1), intArray = testMessage.intArray, stringArray = testMessage.stringArray))
  }
}
