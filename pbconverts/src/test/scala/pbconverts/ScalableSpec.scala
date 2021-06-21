// package pbconverts

// import com.google.protobuf.StringValue
// import org.scalatest.funsuite.AnyFunSuite
// import pbconverts.ConversionTest.{IntMessage, PBPerson}

// class ScalableSpec extends AnyFunSuite {
//   test("Scalable") {
//     // val person = Person(1L, "person name", Some("123456"), Seq("play games", "sing song"))

//     // val builder = PBPerson.newBuilder()
//     // builder.setId(1L)
//     // builder.setName("person name")
//     // builder.setPhone(StringValue.of("123456"))
//     // builder.addHobbies("play games")
//     // builder.addHobbies("sing song")
//     // val personDto = builder.build()

//     // val person1 = Person(
//     //   personDto.getId,
//     //   personDto.getName,
//     //   if (personDto.hasPhone) Some(personDto.getPhone.getValue) else None,
//     //   scala.collection.JavaConverters.iterableAsScalaIterable(personDto.getHobbiesList).toSeq
//     // )

//     // val person2 = Scalable[Person, PBPerson].toScala(builder.build())

//     // assert(person1 == person2)
//   }

// //  test("type alias") {
// //    val pbPerson = PBPerson.newBuilder().setId(1L).build()
// //
// //    locally {
// //      type PPP = Person
// //      val p = Scalable[PPP, PBPerson].toScala(pbPerson)
// //      assert(p.id == 1L)
// //    }
// //
// //    locally {
// //      type PBP = PBPerson
// //      val p = Scalable[Person, PBP].toScala(pbPerson)
// //      assert(p.id == 1L)
// //    }
// //
// //    locally {
// //      type PPP = Person
// //      type PBP = PBPerson
// //      val p = Scalable[PPP, PBP].toScala(pbPerson)
// //      assert(p.id == 1L)
// //    }
// //
// //    locally {
// //      type IM = IntMessage
// //      type MTI = MessageWithType[Int]
// //      val p = Scalable[MTI, IM].toScala(IntMessage.newBuilder().setValue(1).build())
// //      assert(p.value == 1L)
// //    }
// //  }
// }
