package oneparser.xml

import scala.compiletime.{erasedValue, summonFrom, constValue}
import deriving._
import scala.xml._

private[xml] trait DecoderDerivation {

  inline def typeClass[T](ns: NodeSeq): ParseError Either T = summonFrom {
    case s: Decoder[T] => s.fromXml(ns)
  }

  inline def decodeElems[Elems <: Tuple, Labels <: Tuple](n: Int)(ns: NodeSeq, elems: ArrayProduct, format: TagFormat): ParseError Either Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val rootLabel = constValue[label]
            typeClass[elem](ns \ format.formatCase(rootLabel.toString))
              .flatMap { t =>
                elems(n) = t
                decodeElems[elems1, labels1](n + 1)(ns, elems, format)
              }
        }
      case _: Unit => Right(())
    }

  inline def decoderCase[T](ns: NodeSeq, m: Mirror.ProductOf[T], format: TagFormat): ParseError Either T = {
      inline val size = constValue[Tuple.Size[m.MirroredElemTypes]]
      val label = constValue[m.MirroredLabel]
      inline if (size == 0)
        Right(m.fromProduct(EmptyProduct))
      else {
        val elems = new ArrayProduct(size)
        decodeElems[m.MirroredElemTypes, m.MirroredElemLabels](0)(ns, elems, format).map(_ => m.fromProduct(elems))
      }
    }

  inline given derived[T](using ev: Mirror.Of[T], format: TagFormat = TagFormat.LowerCammelCase): Decoder[T] = new Decoder[T] {
    def fromXml(ns: NodeSeq): ParseError Either T = {
      inline ev match {
        case m: Mirror.SumOf[T] => Left(ParseError.UncaughtError("Not implemented for coproducts", "")) // No instances for coproducts for now.
        case m: Mirror.ProductOf[T] => decoderCase(ns, m, format)
      }
    }
  }
}