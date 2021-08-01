package oneparser.xml

import scala.compiletime.{constValue, erasedValue, summonFrom}
import deriving.*
import scala.compiletime.summonInline
import scala.xml.*

private[xml] trait DecoderDerivation {

  inline def decodeElems[Elems <: Tuple, Labels <: Tuple](n: Int)(ns: NodeSeq, elems: Array[Any], format: TagFormat): ParseError Either Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val rootLabel = constValue[label]
            summonInline[Decoder[elem]].fromXml(ns \ format.formatCase(rootLabel.toString))
              .flatMap { t =>
                elems(n) = t
                decodeElems[elems1, labels1](n + 1)(ns, elems, format)
              }
        }
      case _: EmptyTuple => Right(())
    }

  inline def decodeProduct[T](m: Mirror.ProductOf[T], format: TagFormat): Decoder[T] = new Decoder[T] {
    override def fromXml(elem: NodeSeq): Either[ParseError, T] = {
      inline val size = constValue[Tuple.Size[m.MirroredElemTypes]]
      val label = constValue[m.MirroredLabel]
      inline if (size == 0)
      Right(m.fromProduct(EmptyTuple))
      else {
        val elems = new Array[Any](size)
        decodeElems[m.MirroredElemTypes, m.MirroredElemLabels](0)(elem, elems, format)
          .map(_ => m.fromProduct(Tuple.fromArray(elems)))
      }
    }
  }

  inline given derived[T](using ev: Mirror.Of[T], format: TagFormat = TagFormat.LowerCammelCase): Decoder[T] =
    inline ev match {
      case m: Mirror.SumOf[T] => new Decoder[T] {
        override def fromXml(elem: NodeSeq): Either[ParseError, T] = {
          Left(ParseError.UncaughtError("Not implemented for coproducts", "")) // No instances for coproducts for now.
        }
      }
      case m: Mirror.ProductOf[T] => decodeProduct(m, format)
  }
}