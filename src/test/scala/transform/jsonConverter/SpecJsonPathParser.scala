package transform.jsonConverter

import org.json4s.JValue
import transform.jsonConverter.syntaxJsonpath.rule.JsonPathAST
import transform.jsonConverter.syntaxJsonpath.rule.JsonPathParser.compile
import transform.utils.JsonUtil.{JValueWithPower, StringWithJsonPower}

import scala.io.Source.fromResource

object SpecJsonPathParser extends App {

  def show(j: JValue)(s: String) = {

    val p: Either[String, JsonPathAST.Query] = compile(s)

    println("Expr: " + s)
    p.foreach { s =>
      println("AST:  " + s)
      println("Show: " + s.pretty)
    }
    println("=============================")

    val o = p.map(_.extract(j)).map { i =>
      val r = i.map(_.pretty)
      r.mkString(
        s"${i.size} ///////////////////////////////////////////////////////////////////////\n< ",
        " >,\n< ",
        " >\n////////////////////////////////////////////////////////////////////////////////",
      )
    }
    o.foreach(println)

  }

  val ss = List(
    "$[?(.TRT_INFO && .ISR_INFO && .RCPT_HEADER && .FEE_DETAIL && .PRS_INFO)].PRS_INFO[0:]",
    //    "$[?(.TRT_INFO && .ISR_INFO && .RCPT_HEADER && .FEE_DETAIL && .PRS_INFO)]['TRT_INFO', 'ISR_INFO', 'RCPT_HEADER', 'FEE_DETAIL','PRS_INFO']",
    //    "$[?(.TRT_INFO && .ISR_INFO && .RCPT_HEADER && .FEE_DETAIL && .PRS_INFO)]['TRT_INFO'].CUSTOM4",
    //    ".'store'.book[*].'author'",
    //    "$[?(@ ]",
    //    "$..[?(.price)]",
    //    "$.store.book[?(@.price)]",
    //    "$.store..price",
    //
    //    "$..book[*]",       // todo
    //    "$.store.book[1:2]",      // <<<
    //    "$..book[1,2]",
    //
    //    "$..book[-2]",
    //    "$..book[:2]",      // <<<
    //
    //    "$..book[0]",     // <<<
    //    "$..book[-2:]",     // <<<
    //    ".*[?(..tag.price)]",      // <<<
    //    "$..book[?(@.isbn)]",
    //    "$.store..[?(@['price'])]",
    //    "$.store.book[?(@['price'] < 10)]",
    //    "$..book[?(@.price <= $['expensive'])]",
    //    "$..book[?(@.author =~ '(?i).*REES')]",
    //    "$..*",
    //    "$..book",
    //    "$['store']['book'][0]['title']",
    //    "$['store']['book'][0]['title'].*[?( @.name =~ 'abc*' || @.name =~ 'year')]",
    //    "$.store.book[?(@.price <= $['expensive'])]",
    //    "$.this[? ( (@.price < 10 && @.category == 'fiction') || (@.name == 'this') )]",
    //    "$.store.book[-1].title"    // todo
  )

  ////////////////////////////////////////////////////////////////////////////////


  val input0 =
    """
      |{
      |    "store": {
      |        "book": [
      |            {
      |                "category": "reference",
      |                "author": "Nigel Rees",
      |                "title": "Sayings of the Century",
      |                "price": 8.95
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "Evelyn Waugh",
      |                "title": "Sword of Honour",
      |                "price": 12.99
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "Herman Melville",
      |                "title": "Moby Dick",
      |                "isbn": "0-553-21311-3",
      |                "price": 8.99
      |            },
      |            {
      |                "category": "fiction",
      |                "author": "J. R. R. Tolkien",
      |                "title": "The Lord of the Rings",
      |                "isbn": "0-395-19395-8",
      |                "price": 22.99,
      |                "tag": true
      |            }
      |        ],
      |        "bicycle": {
      |            "book" : { "name" : "mybook", "price": 22.99 },
      |            "color": "red",
      |            "price": 19.95
      |        },
      |        "price" : 2000
      |    },
      |    "expensive": 10
      |}
      |""".stripMargin

  val json0 = input0.toJValue

  val path = "lemon\\from.json"
  val jstr = fromResource(path).mkString
  val json = jstr.toJValue

  //  println(json.pretty)



  ss.foreach(show(json))


}
