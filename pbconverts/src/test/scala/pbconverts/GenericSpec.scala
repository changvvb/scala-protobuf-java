package pbconverts

import com.google.protobuf.Int32Value
import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.TestMessagePB

class GenericSpec extends AnyFunSuite {
  test("test generic to proto") {
    val testGenericMessage = TestGenericMessage.intDefault

    val testMessagePB: TestMessagePB = Protoable[TestGenericMessage[Int], TestMessagePB].toProto(testGenericMessage)
    assert(testMessagePB.getGenericValue == testGenericMessage.genericValue)
    assert(testMessagePB.getGenericOpt.getValue == testGenericMessage.genericOpt.get)
  }

  test("test generic to scala") {
    val testMessagePB = TestMessagePB.newBuilder().setGenericValue(9).setGenericOpt(Int32Value.of(9)).build()

    val testGenericMessage: TestGenericMessage[Int] = Scalable[TestGenericMessage[Int], TestMessagePB].toScala(testMessagePB)
    assert(testMessagePB.getGenericValue == testGenericMessage.genericValue)
    assert(testMessagePB.getGenericOpt.getValue == testGenericMessage.genericOpt.get)
  }
}
