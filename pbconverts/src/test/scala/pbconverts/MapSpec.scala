package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.TestMessagePB

class MapSpec extends AnyFunSuite {
  test("test map to protobuf") {
    val testMessage = TestMessage.default

    val testMessagePB: TestMessagePB = Protoable[TestMessage, TestMessagePB].toProto(testMessage)
    assert(testMessagePB.getLongIntKVOrThrow(1L) == 1)
    assert(testMessagePB.getLongStringKVOrThrow(1L) == "string1")
    assert(testMessagePB.getStringIntKVOrThrow("string1") == 1)
    assert(testMessagePB.getStringStringKVOrThrow("string1") == "string1")
  }

  test("test map to scala") {
    val testMessagePB = TestMessagePB
      .newBuilder()
      .putLongIntKV(1, 1)
      .putLongStringKV(2, "string")
      .putStringIntKV("key3", 3)
      .putStringStringKV("key4", "4")
      .build()

    val testMessage: TestMessage = Scalable[TestMessage, TestMessagePB].toScala(testMessagePB)
    assert(testMessage.longIntKV(1) == 1)
    assert(testMessage.longStringKV(2) == "string")
    assert(testMessage.stringIntKV("key3") == 3)
    assert(testMessage.stringStringKV("key4") == "4")
  }

}
