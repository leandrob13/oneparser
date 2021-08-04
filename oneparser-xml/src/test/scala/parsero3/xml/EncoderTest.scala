package oneparser.xml

import scala.io.Source
import scala.xml.{XML, NodeSeq}
import org.junit.Test
import org.junit.Assert._

class EncoderTest {
  import EncoderTest._

  @Test def encodePrimitiveTypes() = {
    val xml = loadFile("/PrimitiveTest.xml")
    val pt = PrimitiveTest("a", 1, 0.1, 2, 0.2, true)

    val enc = Encoder[PrimitiveTest]

    val xmlPt = enc.toXml(pt)
    assertEquals(xmlPt, xml)
  }

  @Test def encodeTypes() = {
    val xml = loadFile("/TypeTest.xml")
    val pt = PrimitiveTest("a", 1, 0.1, 2, 0.2, true)
    val tt = TypeTest(Some("test"), None, List(pt, pt, pt), Some(EnumTest.One), AdtTest.IntAdt(1))
    val enc = Encoder[TypeTest]
    val xmlTt = enc.toXml(tt)

    assertEquals(xmlTt, xml)
  }
}

object EncoderTest {

  def loadFile(path: String): NodeSeq =
    XML.loadString(
      Source
        .fromInputStream(this.getClass.getResourceAsStream(path))
        .mkString
        .replace("\n", "")
        .replaceAll("> +<","><")
    )
}