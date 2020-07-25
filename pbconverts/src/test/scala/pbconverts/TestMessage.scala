package pbconverts

import java.time.ZonedDateTime

case class TestMessage(
    intValue: Int,
    longValue: Long,
    floatValue: Float,
    doubleValue: Double,
    boolValue: Boolean,
    stringValue: String,
    intOpt: Option[Int],
    longOpt: Option[Long],
    floatOpt: Option[Float],
    doubleOpt: Option[Double],
    boolOpt: Option[Boolean],
    stringOpt: Option[String],
    time: ZonedDateTime,
    timeOpt: Option[ZonedDateTime],
    intArray: Array[Int],
    stringArray: Array[String],
    seq: Seq[Double],
    stringStringKV: Map[String, String], // not convert k or v
    longStringKV: Map[Long, String], // convert k, but not v
    stringIntKV: Map[String, Int], // convert v, but not k
    longIntKV: Map[Long, Int] // convert k and v
)

object TestMessage {
  def default =
    TestMessage(
      intValue = 64,
      longValue = 128,
      floatValue = 256.0f,
      doubleValue = 512.0,
      boolValue = true,
      stringValue = "stringValue",
      intOpt = Some(64),
      longOpt = Some(128),
      floatOpt = Some(256.0f),
      doubleOpt = Some(512.0),
      boolOpt = Some(true),
      stringOpt = Some("stringValue"),
      time = ZonedDateTime.now(),
      timeOpt = Some(ZonedDateTime.now()),
      intArray = Array(100, 200, 300),
      stringArray = Array("string1", "string2", "string3"),
      seq = Seq(1.0, 2.0, 3.0),
      stringStringKV = Map("string1" -> "string1", "string2" -> "string2"),
      longStringKV = Map(1L -> "string1", 2L -> "string2"),
      stringIntKV = Map("string1" -> 1, "string2" -> 2),
      longIntKV = Map(1L -> 1, 2L -> 2)
    )

  def zero: TestMessage =
    TestMessage(
      intValue = 0,
      longValue = 0,
      floatValue = 0f,
      doubleValue = 0.0,
      boolValue = false,
      stringValue = "",
      intOpt = None,
      longOpt = None,
      floatOpt = None,
      doubleOpt = None,
      boolOpt = None,
      stringOpt = None,
      time = ZonedDateTime.now(),
      timeOpt = None,
      intArray = Array.empty,
      stringArray = Array.empty,
      seq = Seq.empty,
      stringStringKV = Map.empty,
      longStringKV = Map.empty,
      stringIntKV = Map.empty,
      longIntKV = Map.empty
    )
}
