package oneparser.xml

import scala.compiletime.{erasedValue, summonFrom, constValue}
import deriving._
import scala.xml._

private[xml] trait EncoderDerivation {

  inline def typeClass[T](x: T, label: String, format: TagFormat): NodeSeq = summonFrom {
    case s: Encoder[T] => s.toXml(x, label, format)
  }

  inline def encoderElems[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Any, format: TagFormat, ns: NodeSeq): NodeSeq =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val rootLabel = constValue[label]
            val nodeseq = typeClass(productElement[elem](x, n), rootLabel.toString, format)
            encoderElems[elems1, labels1](n + 1)(x, format, ns ++ nodeseq)
        }
      case _: Unit => ns
    }

  inline def encoderCase[T](x: Any, m: Mirror.ProductOf[T], format: TagFormat): NodeSeq = {
    val label = constValue[m.MirroredLabel]
    inline m match {
      case m: Mirror.Singleton => 
        XML.loadString(s"<$label/>")
      case _ => 
        val tx = encoderElems[m.MirroredElemTypes, m.MirroredElemLabels](0)(x, format, NodeSeq.Empty)
        Elem(null, format.formatCase(label), Null, TopScope, true, tx: _*)
    }
  }

  inline given derived[T](using ev: Mirror.Of[T], format: TagFormat = TagFormat.LowerCammelCase): Encoder[T] = new Encoder[T] {
    def toXml(t: T): NodeSeq = {
      inline ev match {
        case m: Mirror.SumOf[T] => NodeSeq.Empty // No instances for coproducts for now.
        case m: Mirror.ProductOf[T] => encoderCase(t, m, format)
      }
    }
  }
}