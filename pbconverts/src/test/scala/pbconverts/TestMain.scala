package pbconverts

import ConversionTest.PBTestMessage
import ConversionTest.StringMessage

object TestMain extends App {

  // val testMessage = TestMessage.default
  // val pbTestMessage: PBTestMessage = Protoable[TestMessage, PBTestMessage].toProto(testMessage)
  // println(pbTestMessage.getIntValue)

  val stringMessage = MessageWithType("test")
  // val x = Protoable[MessageWithType[String], StringMessage].toProto(stringMessage)
  val x = StringMessage.newBuilder.setValue("Test").build
  val y = Scalable[MessageWithType[String], StringMessage].toScala(x)

  println(x.getValue)
  // println(y)
}
