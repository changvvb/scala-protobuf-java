# scala-protobuf-java

![Scala CI](https://github.com/changvvb/scala-protobuf-java/workflows/Scala%20CI/badge.svg)
[![codecov](https://codecov.io/gh/changvvb/scala-protobuf-java/branch/master/graph/badge.svg)](https://codecov.io/gh/changvvb/scala-protobuf-java)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.changvvb/scala-protobuf-java_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.changvvb/scala-protobuf-java_2.13)


sbt 
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

Define case class `Person`
```scala
case class Person(id: Long, name: String, phone: Option[String], hobbies: Seq[String])
```

Define protobuf message `PersionDto`
```proto
message PersonDto {
    int64 id = 1;
    string name = 2;
    google.protobuf.StringValue phone = 3;
    repeated string hobbies = 4;
}
```
Using protoc, it will generate `PersonDto.java` with corresponding members. You can convert `Person` to `PersonDto` like this
```scala
val builder = PersonDto.newBuilder()
builder.setId(person.id)
builder.setName(person.name)
person.phone.foreach(p => builder.setPhone(StringValue.of(p)))
person.hobbies.foreach(builder.addHobbies)
val personDto:PersonDto = builder.build()
``` 
On the contrary, you can convert `PersonDto` to `Person` like this
```scala
val person1 = Person(
  personDto.getId,
  personDto.getName,
  if(personDto.hasPhone) Some(personDto.getPhone.getValue) else None,
  scala.collection.JavaConverters.iterableAsScalaIterable(personDto.getHobbiesList).toSeq
)
```

Now, we can do it with a few codes with the help of scala-protobuf-java 
```scala
val personDto:PersonDto = Protoable[Person,PersonDto].toProto(person)
val person:Person = Scalable[Person,PersonDto].toScala(personDto)
```
