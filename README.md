# scala-protobuf-java

![Scala CI](https://github.com/changvvb/scala-protobuf-java/workflows/Scala%20CI/badge.svg)
[![codecov](https://codecov.io/gh/changvvb/scala-protobuf-java/branch/master/graph/badge.svg)](https://codecov.io/gh/changvvb/scala-protobuf-java)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.changvvb/scala-protobuf-java_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.changvvb/scala-protobuf-java_2.13)


### What is this
`scala-protobuf-java` is a conversion tool between `scala` types and `protobuf-java` types. It can help impatient you save a lot of codes.
`scala-protobuf-java` use scala macro to generate what you need, so it's type safe at compile time. Now you just keep free to write your code, `scala-proto-java` will check the error at compile time. 

### Dependency
sbt (for `2.12` and `2.13`. If you need more, just submit a issue or PR)
```scala
libraryDependencies += "com.github.changvvb" %% "scala-protobuf-java" % "0.1.1"
```
maven
```xml
<dependency>
  <groupId>com.github.changvvb</groupId>
  <artifactId>scala-protobuf-java_2.13</artifactId>
  <version>0.1.1</version>
</dependency>
```

### How to use

#### Basic usage
Define case class `Person`
```scala
case class Person(id: Long, name: String, phone: Option[String], hobbies: Seq[String])
```

Define protobuf message `PBPerson`
```proto
message PBPerson {
    int64 id = 1;
    string name = 2;
    google.protobuf.StringValue phone = 3;
    repeated string hobbies = 4;
}
```
Using protoc, it will generate `PBPerson.java` with corresponding members. You can convert `Person` to `PBPerson` like this
```scala
val builder = PBPerson.newBuilder()
builder.setId(person.id)
builder.setName(person.name)
person.phone.foreach(p => builder.setPhone(StringValue.of(p)))
person.hobbies.foreach(builder.addHobbies)
val pbPerson:PBPerson = builder.build()
``` 
On the contrary, you can convert `PBPerson` to `Person` like this
```scala
val person1 = Person(
  pbPerson.getId,
  pbPerson.getName,
  if(pbPerson.hasPhone) Some(pbPerson.getPhone.getValue) else None,
  scala.collection.JavaConverters.iterableAsScalaIterable(pbPerson.getHobbiesList).toSeq
)
```

Now, we can do it with a few codes with the help of scala-protobuf-java 
```scala
import pbconverts.{ Protoable, Scalable }
val convertedPBPerson:PBPerson = Protoable[Person,PBPerson].toProto(person)
val convertedPerson:Person = Scalable[Person,PBPerson].toScala(pbPerson)
```
More simplified
```scala
import pbconverts.ProtoScalable
val protoScalable = ProtoScalble[Person,PBPerson]
protoScalable.toProto(person)
protoScalable.toScala(pbPerson)
```
Or you can use `implicit` style
```scala
import pbconverts.{ ProtoScalable, Converter}
implicit val protoScalable = ProtoScalble[Person,PBPerson]
Converter.toProto(person)
```
In general, we often put the `implicit` value in companion object
```scala
object Person {
  implicit val protoScalable = ProtoScalble[Person,PBPerson]
}
Converter.toScala(pbPerson)
```

#### Nested structure
Define a nested case class `ParentMessage`
```scala
case class SubMessage(subValue:Int)
case class ParentMessage(parentValue:Int, subMessage:SubMessage)
```

Define a nested protobuf message `PBParentMessage`
```scala
message PBSubMessage {
  int32 sub_value = 1;
}

message PBParentMessage {
  int32 parent_value = 1;
  PBSubMessage sub_message = 2;
}
```

```scala
import pbconverts.{ Scalable, Protoable }
implicit val subProtoable = Protoable[SubMessage,PBSubMessage]
Protoable[ParentMessage, PBParentMessage].toProto(ParentMessage(...))


implicit val subScalable = Scalable[SubMessage,PBSubMessage]
Scalable[ParentMessage, PBParentMessage].toScala(PBParentMessage.newBuilder().build())
```

> Hint: you can replace `Protoable[SubMessage,PBSubMessage]` and `Scalable[SubMessage,PBSubMessage]` with `ProtoScalable[SubMessage,PBSubMessage]`

#### Custom you own conversion
If you want use `Protoable[Person, PBPerson]` to convert a `Person` object to `PBPerson` object but you want custom field `id`.
You can use `ProtoableBuilder`:
```scala
val customProtoable = ProtoableBuilder[Person,PBPerson]
                        .setField(_.getId, p => if (p.id < 0) 0 else p.id)
                        .build
customProtoable.toProto(Person(...))
```
Also, if you want convert protobuf to case class with the same logic, you can use `ScalableBuilder`
```scala
val customScalable = ScalableBuilder[Person,PBPerson]
                       .setField(_.id, p => if(p.getId < 0) 0 else p.id)
                       .build
customScalable.toScala(PBPerson.newBuilder().build)
```
