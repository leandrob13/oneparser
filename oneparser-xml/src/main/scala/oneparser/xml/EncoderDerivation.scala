package oneparser.xml

import scala.compiletime.{constValue, erasedValue, summonFrom, summonInline}
import scala.deriving.*
import scala.xml.*

private[xml] trait EncoderDerivation {

  inline def encoderElems[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Product, format: TagFormat, ns: NodeSeq): NodeSeq =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val rootLabel = constValue[label]
            val nodeseq = summonInline[Encoder[elem]].toXml(x.productElement(n).asInstanceOf[elem], rootLabel.toString, format)//typeClass(x, rootLabel.toString, format)
            encoderElems[elems1, labels1](n + 1)(x, format, ns ++ nodeseq)
        }
      case _: EmptyTuple => ns
    }

  inline def encodeProduct[T](m: Mirror.ProductOf[T], format: TagFormat): Encoder[T] = new Encoder[T] {
    override def toXml(t: T): NodeSeq = {
      val label = constValue[m.MirroredLabel]
      inline m match {
        case m: Mirror.Singleton =>
          XML.loadString(s"<$label/>")
        case _ =>
          val tx = encoderElems[m.MirroredElemTypes, m.MirroredElemLabels](0)(t.asInstanceOf[Product], format, NodeSeq.Empty)
          Elem(null, format.formatCase(label), Null, TopScope, true, tx: _*)
      }
    }
  }

  inline given derived[T](using ev: Mirror.Of[T], format: TagFormat = TagFormat.LowerCammelCase): Encoder[T] = {
    inline ev match {
      case m: Mirror.SumOf[T] =>
        new Encoder[T] {
          override def toXml(t: T): NodeSeq = NodeSeq.Empty
        }
      case m: Mirror.ProductOf[T] => encodeProduct(m, format)
    }
  }
}