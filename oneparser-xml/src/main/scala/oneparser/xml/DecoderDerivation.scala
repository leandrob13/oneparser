package oneparser.xml

import scala.compiletime.{constValue, erasedValue, summonFrom, summonAll}
import deriving.*
import scala.collection.mutable.ListBuffer
import scala.compiletime.summonInline
import scala.xml.*

private[xml] trait DecoderDerivation {

  inline def decodeProductElems[Elems <: Tuple, Labels <: Tuple](n: Int)(ns: NodeSeq, elems: Array[Any], format: TagFormat): ParseError Either Unit =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val rootLabel = constValue[label]
            summonInline[Decoder[elem]].fromXml(ns \ format.formatCase(rootLabel.toString))
              .flatMap { t =>
                elems(n) = t
                decodeProductElems[elems1, labels1](n + 1)(ns, elems, format)
              }
        }
      case _: EmptyTuple => Right(())
    }

  inline def decodeProduct[T](m: Mirror.ProductOf[T], format: TagFormat): Decoder[T] = new Decoder[T] {
    override def fromXml(elem: NodeSeq): Either[ParseError, T] = {
      inline val size = constValue[Tuple.Size[m.MirroredElemTypes]]
      val label = constValue[m.MirroredLabel]
      inline if (size == 0) Right(m.fromProduct(EmptyTuple))
      else {
        val elems = new Array[Any](size)
        decodeProductElems[m.MirroredElemTypes, m.MirroredElemLabels](0)(elem, elems, format)
          .map(_ => m.fromProduct(Tuple.fromArray(elems)))
      }
    }
  }

  inline def decodeCoproductElems[Elems <: Tuple, Labels <: Tuple](ls: ListBuffer[Decoder[_]]): ListBuffer[Decoder[_]] =
    inline erasedValue[Elems] match {
      case _: (elem *: elems1) =>
        inline erasedValue[Labels] match {
          case _: (label *: labels1) =>
            val rootLabel = constValue[label]
            decodeCoproductElems[elems1, labels1](ls += summonInline[Decoder[elem]])
        }
      case _: EmptyTuple => ls
    }

  inline def decodeCoproduct[T](m: Mirror.SumOf[T], format: TagFormat): Decoder[T] = new Decoder[T] {
    override def fromXml(elem: NodeSeq): Either[ParseError, T] = {
      inline val size = constValue[Tuple.Size[m.MirroredElemTypes]]
      val label = constValue[m.MirroredLabel]
      val elems = new Array[Any](size)
      decodeCoproductElems[m.MirroredElemTypes, m.MirroredElemLabels](ListBuffer.empty)
        .toList
        .map(_.asInstanceOf[Decoder[T]].fromXml(elem))
        .collectFirst {
        case v @ Right(_) => v
      }.getOrElse(Left(ParseError.ConvertingError("No suitable match for coproduct", label)))
    }
  }

  inline given derived[T](using ev: Mirror.Of[T], format: TagFormat = TagFormat.LowerCammelCase): Decoder[T] =
    inline ev match {
      case m: Mirror.SumOf[T] => decodeCoproduct(m, format)
      case m: Mirror.ProductOf[T] => decodeProduct(m, format)
  }
}