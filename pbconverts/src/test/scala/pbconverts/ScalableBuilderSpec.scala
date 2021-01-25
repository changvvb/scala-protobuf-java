package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.PBTestMessage

class ScalableBuilderSpec extends AnyFunSuite {
  test("test ScalableBuilder") {
    assertTestMessage(TestMessage.default)
    assertTestMessage(TestMessage.zero)
  }

  def assertTestMessage(testMessage: TestMessage) = {
    val pbTestMessage = Protoable[TestMessage, PBTestMessage].toProto(testMessage)
    val testMessage2 = ScalableBuilder[TestMessage, PBTestMessage]
      .setField(_.intOpt, m => if (m.hasIntOpt) Some(m.getIntOpt.getValue + 1) else None)
      .build
      .toScala(pbTestMessage)

    assert(testMessage2.intArray.sameElements(testMessage.intArray))
    assert(testMessage2.stringArray.sameElements(testMessage.stringArray))
    assert(testMessage2.personArray.sameElements(testMessage.personArray))

    assert(
      testMessage == testMessage2.copy(
        intOpt = testMessage2.intOpt.map(_ - 1),
        intArray = testMessage.intArray,
        stringArray = testMessage.stringArray,
        personArray = testMessage.personArray
      )
    )
  }
}
