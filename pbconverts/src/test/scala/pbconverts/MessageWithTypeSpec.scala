package pbconverts

import org.scalatest.funsuite.AnyFunSuite
import pbconverts.ConversionTest.{IntMessage, StringMessage}

class MessageWithTypeSpec extends AnyFunSuite {

  test("test message with type parameter") {
    locally {
      val intMessage = MessageWithType(5)
      val proto = Protoable[MessageWithType[Int], IntMessage].toProto(intMessage)
      assert(proto.getValue == 5)

      val intMessage2 = Scalable[MessageWithType[Int], IntMessage].toScala(proto)
      assert(intMessage == intMessage2)
    }

    locally {
      val stringMessage = MessageWithType("test")
      val proto = Protoable[MessageWithType[String], StringMessage].toProto(stringMessage)
      assert(proto.getValue == "test")

      val stringMessage2 = Scalable[MessageWithType[String], StringMessage].toScala(proto)
      assert(stringMessage == stringMessage2)
    }

  }

}
