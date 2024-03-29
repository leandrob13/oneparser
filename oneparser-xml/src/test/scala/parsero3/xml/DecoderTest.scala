package oneparser.xml

import scala.io.Source
import scala.xml.{XML, NodeSeq}
import org.junit.Test
import org.junit.Assert._

class DecoderTest {
  import DecoderTest._

  @Test def decodePrimitiveTypes() = {
    val xml = loadFile("/PrimitiveTest.xml")
    val pt = PrimitiveTest("a", 1, 0.1, 2, 0.2, true)
    val dec = Decoder[PrimitiveTest]
    val xmlTopt = dec.fromXml(xml)

    assertEquals(xmlTopt, Right(pt))
  }

  @Test def decodeTypes() = {
    val xml = loadFile("/TypeTest.xml")
    val pt = PrimitiveTest("a", 1, 0.1, 2, 0.2, true)
    val tt = TypeTest(Some("test"), None, List(pt, pt, pt), Some(EnumTest.One), AdtTest.IntAdt(1))
    val dec = Decoder[TypeTest]
    val xmlTtopt = dec.fromXml(xml)

    assertEquals(xmlTtopt, Right(tt))
  }
}

object DecoderTest {

  def loadFile(path: String): NodeSeq =
    XML.loadString(
      Source
        .fromInputStream(this.getClass.getResourceAsStream(path))
        .mkString
        .replace("\n", "")
        .replaceAll("> +<","><")
    )
}