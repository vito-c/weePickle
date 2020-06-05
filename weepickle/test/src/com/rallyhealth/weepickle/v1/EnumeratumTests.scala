package com.rallyhealth.weepickle.v1

import com.rallyhealth.weejson.v1.{Num, WeeJson}
import com.rallyhealth.weepickle.v1.WeePickle.FromTo
import com.rallyhealth.weepickle.v1.WeePickle.To

import utest._

import scala.reflect.{ClassTag, classTag}
import com.rallyhealth.weepickle.v1.WeePickle.FromScala
import com.rallyhealth.weepickle.v1.WeePickle.ToScala
import com.rallyhealth.weejson.v1.jackson.FromJson
import com.rallyhealth.weejson.v1.jackson.ToJson

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
    WeePickle.FromTo.join(WeePickle.ToString, WeePickle.FromString)
    // fromTo[String]
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

object Fruit {


  case object Peach extends Fruit("peach")

  case object Pear extends Fruit("pear")

  case object Strawberry extends Fruit("strawberry")

  // override val values = Seq(Peach, Pear, Strawberry)
}
object PickledFruit extends WeePickleEnum[Fruit] {
}

object EnumeratumTests extends TestSuite {


  val tests = Tests {

    test("to") {
      test("Fruit") {
        import PickledFruit.peachPickler
        implicitly[To[Fruit]].visitString("peach") ==> Fruit.Peach
      }
      test("Peach type") {
        import PickledFruit.peachPickler
        val peachy = FromJson("""""peach"""").transform(ToScala[Fruit.Peach.type])
        peachy ==> Fruit.Peach
        val pick = FromScala(Fruit.Peach).transform(ToJson.string)
        pick ==> """"peach""""
        // implicitly[To[Fruit.Peach.type]].visitString("peach") ==> Fruit.Peach
      }
      test("Peach") {
        import PickledFruit.peachPickler
        // Explicitly call macro
        // implicit val peachFromTo = WeePickle.macroSingletonFromTo[Fruit.Peach.type]
        // println(peachFromTo)
        // implicitly[To[Fruit]].visitString("peach") ==> Fruit.Peach
        macrolizer.show {
          implicitly[To[Fruit.Peach.type]].visitString("peach") ==> Fruit.Peach
        }
      }
    }
  }
}
