package oneparser.xml

case class PrimitiveTest(a: String, b: Int, c: Float, d: Long, e: Double, f: Boolean)

enum CoproductTest[+T] {
  case Number(v: Int) extends CoproductTest[Int]
  case Character(v: String) extends CoproductTest[String]
}

enum EnumTest(x: Int, s: String) {
  case One extends EnumTest(1, "int")
  case Two extends EnumTest(2, "int")
}

enum AdtTest {
  case IntAdt(i: Int) extends AdtTest
  case FloatAdt(f: Float) extends AdtTest
}

case class TypeTest(
  name: Option[String],
  p: Option[PrimitiveTest],
  ps: List[PrimitiveTest],
  enumerator: Option[EnumTest],
  adt: AdtTest
)
