package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.TestMessageDto

class ProtoableBuilderSpec extends AnyFunSuite {
  test("test map") {
    val testMessage                    = TestMessage(1, "name", Some("desc"), Map("key" -> "value"), Map.empty, Map.empty)
    val testMessageDto: TestMessageDto = Protoable[TestMessage, TestMessageDto].toProto(testMessage)
    assert(testMessageDto.getId == testMessage.id)
    assert(testMessageDto.getStringStringAttrsOrThrow("key") == "value")

    val testMessage2 = Scalable[TestMessage, TestMessageDto].toScala(testMessageDto)
    assert(testMessage == testMessage2)
  }

}
