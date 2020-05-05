package com.rallyhealth.weepickle.v1

import com.rallyhealth.weejson.v1.{Num, WeeJson}
import com.rallyhealth.weepickle.v1.WeePickle._
import utest._

import scala.reflect.{ClassTag, classTag}

// enumeratum stuff
trait EnumEntry {

  def entryName: String
}

trait Enum[T <: EnumEntry] {

  def values: Seq[T]
}

trait WeePickleEnum[Fruit <: EnumEntry] extends Enum[Fruit] {

  implicit def peachPickler[Peach <: Fruit : ClassTag]: WeePickle.FromTo[Peach] = {
    val expectedType = classTag[Peach].runtimeClass.getSimpleName
    fromTo[String]
      .bimap[Peach](
        _.entryName,
        s => values.find(_.entryName == s).get match {
          case peach: Peach => peach
          case notPeach => throw new Exception(s"Expected $expectedType, got $notPeach")
        }
      )
  }
}

sealed abstract class Fruit(override val entryName: String) extends EnumEntry

object Fruit extends WeePickleEnum[Fruit] {


  case object Peach extends Fruit("peach")

  case object Pear extends Fruit("pear")

  case object Strawberry extends Fruit("strawberry")

  override val values = Seq(Peach, Pear, Strawberry)
}

object EnumeratumTests extends TestSuite {


  val tests = Tests {

    test("to") {
//      test("Fruit") {
//        implicitly[To[Fruit]].visitString("peach") ==> Fruit.Peach
//      }
      test("Peach") {
        // Explicitly call macro
        implicit val peachFromTo = WeePickle.macroSingletonFromTo[Fruit.Peach.type]
        println(peachFromTo)
        implicitly[To[Fruit.Peach.type]].visitString("peach") ==> Fruit.Peach
      }
    }
  }
}
