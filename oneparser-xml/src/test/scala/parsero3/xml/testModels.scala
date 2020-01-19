package oneparser.xml

case class PrimitiveTest(a: String, b: Int, c: Float, d: Long, e: Double, f: Boolean)

case class TypeTest(name: Option[String], primitive: Option[PrimitiveTest], primitives: List[PrimitiveTest])