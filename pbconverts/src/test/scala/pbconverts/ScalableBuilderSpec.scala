package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.TestMessageDto

class ScalableBuilderSpec extends AnyFunSuite {
  test("test map") {
    val testMessage = TestMessage(1, "name", Some("desc"), Map("key" -> "value"), Map.empty, Map.empty)
    val testMessageDto = Protoable[TestMessage, TestMessageDto].toProto(testMessage)
    val testMessage2 = ScalableBuilder[TestMessage, TestMessageDto]
      .setField(_.desc, m => if (m.hasDesc) Some("updated " + m.getDesc.getValue) else None)
      .build
      .toScala(testMessageDto)

    assert(testMessage2.desc.contains("updated desc"))
    assert(testMessage2.copy(desc = Some("desc")) == testMessage)
  }

}
