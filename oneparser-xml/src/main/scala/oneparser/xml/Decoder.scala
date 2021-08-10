package oneparser.xml

import scala.xml._
import scala.util.Try

trait Decoder[T] {
  def fromXml(elem: NodeSeq): ParseError Either T
}

object Decoder extends DecoderDerivation {
  import ParseError._
  
  def apply[T](using c: Decoder[T]): Decoder[T] = c

  def instance[T](f: NodeSeq => ParseError Either T): Decoder[T] =
    f(_)

  def parse[T](elem: NodeSeq)(f: String => T): ParseError Either T = {
    val label = elem.theSeq.toList.headOption.fold("")(_.label)
    if (elem.flatMap(_.child).isEmpty) Left(EmptyElementError(s"Empty element error", label))
    else {
      val s = elem.text.trim
      Try(f(s)).toEither.left.map(_ => ConvertingError(s"Error converting {$s} value for $label", label))
    }
  }

  implicit val stringFromXml: Decoder[String] =
    instance(parse(_)(identity))

  implicit val intFromXml: Decoder[Int] =
    instance(parse(_)(_.toInt))

  implicit val longFromXml: Decoder[Long] =
    instance(parse(_)(_.toLong))

  implicit val floatFromXml: Decoder[Float] =
    instance(parse(_)(_.toFloat))

  implicit val doubleFromXml: Decoder[Double] =
    instance(parse(_)(_.toDouble))

  implicit val booleanFromXml: Decoder[Boolean] = instance[Boolean] { n =>
    val label = n.theSeq.toList.headOption.fold("")(_.label)
    Try {
      val s = n.text.trim
      if (s.equalsIgnoreCase("y") || s.equalsIgnoreCase("n")) s.equalsIgnoreCase("y")
      else s.toBoolean
    }.toEither.left.map(ex => ConvertingError(s"Error converting Boolean value: ${ex.getMessage}", label))
  }

  given optionFromXml[T](using fx: Decoder[T]): Decoder[Option[T]] = instance { ns =>
    if (ns.flatMap(_.child).isEmpty) Right(None)
    else fx.fromXml(ns).map(Some(_))
  }

  given listFromXml[T](using fx: Decoder[T]): Decoder[List[T]] = instance { ns =>
    val zero: Either[ParseError, List[T]] = Right(Nil)
    ns.flatMap(_.child).foldLeft(zero) { (listF, n) =>
      n match {
        case _: Elem =>
          for {
            list <- listF
            f    <- fx fromXml n
          } yield list :+ f
        case _ => listF
      }
    }
  }

  given mutableSeqFromXml[T](using fx: Decoder[T]): Decoder[Seq[T]] = instance { ns =>
    val zero: Either[ParseError, Seq[T]] = Right(Nil)
    ns.flatMap(_.child).foldLeft(zero) { (listF, n) =>
      for {
        list <- listF
        f    <- fx fromXml n
      } yield list :+ f
    }
  }
}