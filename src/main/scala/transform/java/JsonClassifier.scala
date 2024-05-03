package transform.java

import org.json4s.JString
import org.json4s.JsonAST.JObject
import transform.jsonClassify.patternMatching.{MultiPattern, Occurrence}
import transform.utils.JsonUtil.JValueWithPower

import scala.jdk.CollectionConverters.{MapHasAsScala, SeqHasAsJava}


case class JsonClassifier(mp: Either[JObject, MultiPattern]) extends ShowStatus {

  override def isFail(): Boolean = mp.isLeft
  override def isSuccess(): Boolean = mp.isRight
  override def getFailReason(): String = mp.fold( _.pretty, _ => "")
  override def show(): String = mp.fold( _.pretty, m => m.toString())

  def search(s: String) = {
    val ret = mp.map( _.search(s) )

    ClassifyResult(ret)

  }
}

object JsonClassifier {

  def apply( rules: java.util.Map[java.lang.Integer, String], ignore: Boolean = false): JsonClassifier = {


    val s = rules.asScala.map( kv => kv._1.toInt -> kv._2).toSeq
    val mp = MultiPattern(s:_*)

    val err = if( !ignore) {
      val k0 = mp.rules.keys.toSet
      s.filter( kv => !k0.contains(kv._1)).toList
    } else Nil

    val r = Either.cond( err.isEmpty,
      mp,
      JObject( err.map(kv => kv._1.toString -> JString(kv._2) )))

    JsonClassifier(r)
  }

}

case class ClassifyResult(ret: Either[JObject, Iterable[(Int, String, Seq[Occurrence])]]) extends ShowStatus {

  override def isFail(): Boolean = ret.isLeft
  override def isSuccess(): Boolean = ret.isRight
  override def getFailReason(): String = ret.fold( _.pretty, _ => "")
  override def show(): String = ret.fold( _.pretty, _.map(_._2).mkString("[", ",", "]"))

  def getIds(): java.util.List[Integer] = ret.fold( _ => Nil, _.map(i => Integer.valueOf(i._1)).toList).asJava
  def getRules(): java.util.List[String] = ret.fold( _ => Nil, _.map(_._2).toList).asJava

}

