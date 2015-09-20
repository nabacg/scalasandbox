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
                      JObj(
                      Map("type" -> JStr("Home"),
                          "number" -> JStr("34234324324"))),

                      JObj(Map("type" -> JStr("Work"),
                                        "number" -> JStr("29020-93892-038")))))))