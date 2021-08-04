package oneparser.xml

import scala.collection.mutable.ListBuffer
import scala.compiletime.{constValue, erasedValue, summonAll, summonFrom, summonInline}
import scala.deriving.*
import scala.xml.*

private[xml] trait EncoderDerivation {

  inline def encodeProductElems[Elems <: Tuple, Labels <: Tuple](n: Int)(x: Product, format: TagFormat, ns: NodeSeq): NodeSeq =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val rootLabel = constValue[label]
            val nodeseq = summonInline[Encoder[elem]].toXml(x.productElement(n).asInstanceOf[elem], rootLabel.toString, format)//typeClass(x, rootLabel.toString, format)
            encodeProductElems[elems1, labels1](n + 1)(x, format, ns ++ nodeseq)
        }
      case _: EmptyTuple => ns
    }

  inline def encodeProduct[T](m: Mirror.ProductOf[T], format: TagFormat): Encoder[T] = new Encoder[T] {
    override def toXml(t: T): NodeSeq = {
      val label = format.formatCase(constValue[m.MirroredLabel])
      inline m match {
        case m: Mirror.Singleton =>
          XML.loadString(s"<$label>${t.toString}</$label>")
        case _ =>
          val tx = encodeProductElems[m.MirroredElemTypes, m.MirroredElemLabels](0)(t.asInstanceOf[Product], format, NodeSeq.Empty)
          Elem(null, label, Null, TopScope, true, tx: _*)
      }
    }
  }

  inline def encodeCoproductElems[Elems <: Tuple, Labels <: Tuple](ls: ListBuffer[(String, Encoder[_])]): ListBuffer[(String, Encoder[_])] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val rootLabel = constValue[label].toString
            val t = (rootLabel, summonInline[Encoder[elem]])
            encodeCoproductElems[elems1, labels1](ls += t)
        }
      case _: EmptyTuple => ls
    }

  inline def encodeCoproduct[T](m: Mirror.SumOf[T], format: TagFormat): Encoder[T] = new Encoder[T] {
    override def toXml(t: T): NodeSeq = {
      val label = constValue[m.MirroredLabel]
      val ord = m.ordinal(t)
      val (rootLabel: String, enc: Encoder[_]) =
        encodeCoproductElems[m.MirroredElemTypes, m.MirroredElemLabels](ListBuffer.empty)(ord)
      val tx = enc.asInstanceOf[Encoder[T]].toXml(t)

      Elem(null, format.formatCase(label), Null, TopScope, true, tx: _*)
    }
  }

  inline given derived[T](using ev: Mirror.Of[T], format: TagFormat = TagFormat.LowerCammelCase): Encoder[T] = {
    inline ev match {
      case m: Mirror.SumOf[T] => encodeCoproduct(m, format)
      case m: Mirror.ProductOf[T] => encodeProduct(m, format)
    }
  }
}