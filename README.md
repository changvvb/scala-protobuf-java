# scala-protobuf-java

![Scala CI](https://github.com/changvvb/scala-protobuf-java/workflows/Scala%20CI/badge.svg)
[![codecov](https://codecov.io/gh/changvvb/scala-protobuf-java/branch/master/graph/badge.svg)](https://codecov.io/gh/changvvb/scala-protobuf-java)

example
```scala
class MapSpec extends AnyFunSuite {
  test("test map") {
    val testMessage = TestMessage(1,"name",Some("desc"),Map("key" -> "value"),Map.empty,Map.empty)
    val testMessageDto: TestMessageDto = Protoable[TestMessage,TestMessageDto].toProto(testMessage)
    assert(testMessageDto.getId == testMessage.id)
    assert(testMessageDto.getStringStringAttrsOrThrow("key") == "value")

    val testMessage2 = Scalable[TestMessage,TestMessageDto].toScala(testMessageDto)
    assert(testMessage == testMessage2)
  }

}
```
