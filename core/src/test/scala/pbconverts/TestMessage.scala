package pbconverts

import com.google.protobuf.StringValue

case class TestMessage(id: Long, name: String, desc: Option[String],
                       stringStringAttrs:   Map[String, String],
                       longStringAttrs:     Map[Long, String],
                       intStringValueAttrs: Map[Int, StringValue])
