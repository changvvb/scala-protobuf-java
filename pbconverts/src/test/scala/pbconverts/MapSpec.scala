package pbconverts

import com.google.protobuf.ByteString
import org.scalatest.funsuite.AnyFunSuite
import ConversionTest.{PBTestBytes, PBTestMessage}

class MapSpec extends AnyFunSuite {
  test("test map") {
    val testMessage = TestMessage.default

    val pbTestMessage: PBTestMessage = Protoable[TestMessage, PBTestMessage].toProto(testMessage)
    println(pbTestMessage)
    assert(pbTestMessage.getIntValue == testMessage.intValue)
    assert(pbTestMessage.getLongIntKVOrThrow(1L) == 1)
    assert(pbTestMessage.getLongStringKVOrThrow(1L) == "string1")
    assert(pbTestMessage.getStringIntKVOrThrow("string1") == 1)
    assert(pbTestMessage.getStringStringKVOrThrow("string1") == "string1")
  }

//  case class TestBytes(m: Map[String, Int])
//  test("test conversion between bytes in protobuf and Map in case class") {
//    val pbTest1 = ProtoableBuilder[TestBytes, PBTestBytes]
//      .setField(_.getTestBytes, x => ByteString.copyFromUtf8(x.m.keys.mkString(",")))
//      .build
//      .toProto(TestBytes(Map("test" -> 1, "bytes" -> 1)))
//
//    assert(pbTest1.getTestBytes.toStringUtf8 == "test,bytes")
//
//    val test = ScalableBuilder[TestBytes, PBTestBytes]
//      .setField(_.m, _.getTestBytes.toStringUtf8.split(",").map(_ -> 1).toMap)
//      .build
//      .toScala(PBTestBytes.newBuilder().setTestBytes(ByteString.copyFromUtf8("test,bytes")).build())
//
//    assert(test.m == Map("test" -> 1, "bytes" -> 1))
//  }
}
