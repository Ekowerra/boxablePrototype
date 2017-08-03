package functionalProgramming.chapter2.parser


trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[ParserError,Parser[+_]](P: Parsers[ParserError,Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P._
    implicit def tok(s: String) = token(P.string(s))

    def array = surround(string("["),string("]"))(
      value sep string(",") map (vs => JArray(vs.toIndexedSeq))) scope "array"
    def obj = surround(string("{"),string("}"))(
      keyval sep string(",") map (kvs => JObject(kvs.toMap))) scope "object"
    def keyval = escapedQuoted ** (string(":") *> value)
    def lit = scope("literal") {
      string("null").as(JNull) |
        double.map(JNumber(_)) |
        escapedQuoted.map(JString(_)) |
        string("true").as(JBool(true)) |
        string("false").as(JBool(false))
    }
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (obj | array))
  }
}
