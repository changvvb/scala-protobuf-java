package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.TestMessageDto

class MapSpec extends AnyFunSuite {
  test("test map") {
    val testMessage = TestMessage.default

    val testMessageDto: TestMessageDto = Protoable[TestMessage, TestMessageDto].toProto(testMessage)
    assert(testMessageDto.getIntValue == testMessage.intValue)
    assert(testMessageDto.getLongIntKVOrThrow(1L) == 1)
    assert(testMessageDto.getLongStringKVOrThrow(1L) == "string1")
    assert(testMessageDto.getStringIntKVOrThrow("string1") == 1)
    assert(testMessageDto.getStringStringKVOrThrow("string1") == "string1")
  }

}
