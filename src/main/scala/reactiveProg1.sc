abstract class JSON

case class JSeq(elems: List[JSON]) extends JSON
case class JObj(bindings: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(b : Boolean) extends JSON
case object JNull extends JSON

val data = JObj(Map("firstName" -> JStr("John"),
    "lastName" -> JStr("Smith"),
    "address" -> JObj(Map("streetAddress" -> JStr("21 2nd Street"),
                      "state" -> JStr("NY"),
                      "postalCode" -> JNum(10021))),
    "phoneNumbers" ->
                  JSeq(
                    List(
                      JObj(Map("type" -> JStr("Home"),
                          "number" -> JStr("34234324324"))),
                      JObj(Map("type" -> JStr("Work"),
                          "number" -> JStr("29020-93892-038")))))))



def show(json: JSON) : String =
  json match {
    case JNull => "null"
    case JBool(b) => b.toString
    case JStr(s)  => s
    case JNum(n)  => n.toString
    case JSeq(s)  => "[" + s.map(j => show(j)).mkString(",") + "]"
    case JObj(bindings)  => val kvStr = bindings.map( {
        case (k, v) => k + ": " +   show(v)
      })
      "{" + kvStr.mkString(", ") + "}"
  }
show(data)

/*
println("HERe")
for {
  JObj(b) <- data
} yield (b("firstName"), b("lastName"))

println("tehre")

for {
  JObj(bindings) <- data
  JSeq(phones) = bindings("phoneNumbers")
  JObj(phone) <- phones
  JStr(digits) = phone("number")
  if digits startsWith "290"
} yield (bindings("firstName"), bindings("lastName"))
*/

val fun : PartialFunction[String,String] = { case "ping" => "pong"}
if(fun.isDefinedAt("ping"))  fun("ping")
if(fun.isDefinedAt("test"))
  fun("test")

val fTest : PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: y :: rest => "two"
}

fTest.isDefinedAt(List(1,2,3))
fTest.isDefinedAt(List(1))



