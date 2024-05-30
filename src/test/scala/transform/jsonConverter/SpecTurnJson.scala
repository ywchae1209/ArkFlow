package transform.jsonConverter

import transform.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}

import scala.io.Source.fromResource

////////////////////////////////////////////////////////////////////////////////////////////////////
object SpecTurnJson extends App {

  val jstr = fromResource("lemon\\convert.json").mkString

  ////////////////////////////////////////////////////////////////////////////////
  val jv = jstr.toJValue
  //  println(jv)
  val ret = TurnJson(jv)
  println(ret.fold(_.pretty, _.pretty))
  //  ret.foreach(_.show())


  ////////////////////////////////////////////////////////////////////////////////
  val path0 = "lemon\\from.json"
  val jstr0 = fromResource(path0).mkString
  val jv0 = jstr0.toJValue
//  println(s"fromResource ${jv0.pretty}")
  //////////////////////////////////////////////////////////////////////////////
  ret.map{ tj =>
    val ret0 = tj.convert(jv0, jv0)
    println(ret0.fold(_.pretty, _.pretty))
  }

}
