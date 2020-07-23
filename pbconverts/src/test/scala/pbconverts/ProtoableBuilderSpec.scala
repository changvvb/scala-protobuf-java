package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.TestMessageDto

class ProtoableBuilderSpec extends AnyFunSuite {
  test("test map") {
    val testMessage = TestMessage(1, "name", Some("desc"), Map("key" -> "value"), Map.empty, Map.empty)
    val testMessageDto: TestMessageDto = ProtoableBuilder[TestMessage, TestMessageDto]
      .setField(_.getDesc, _.desc.map("updated " + _))
      .build
      .toProto(testMessage)

    assert(testMessageDto.getDesc.getValue == "updated desc")

    val testMessage2 = Scalable[TestMessage, TestMessageDto].toScala(testMessageDto)
    assert(testMessage.copy(desc = Some("updated desc")) == testMessage2)
  }

}
