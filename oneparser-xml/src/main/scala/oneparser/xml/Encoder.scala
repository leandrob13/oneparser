package oneparser.xml

import scala.xml._

trait Encoder[T] {
  def toXml(t: T): NodeSeq

  def toXml(t: T, rootLabel: String, format: TagFormat): NodeSeq = {
    val tx = toXml(t)
    val label = format.formatCase(rootLabel)
    tx \\ format.formatCase(t) match {
      case NodeSeq.Empty =>
        Elem(null, label, Null, TopScope, true, tx: _*)
      case ns =>
        Elem(null, label, Null, TopScope, true, ns.flatMap(_.child): _*)
    }
  }
}

object Encoder extends EncoderDerivation {

  def apply[T](using c: Encoder[T]): Encoder[T] = c

  def instance[T](f: T => NodeSeq): Encoder[T] = f(_)

  def print(ns: NodeSeq, printEmpty: Boolean = true, width: Int = 500, step: Int = 2): String = {
    val pp  = new PrettyPrinter(width, step)
    val nns = if (printEmpty) ns else ns.filter(_.nonEmpty)
    nns.map(pp.format(_)).foldLeft("")(_ + _)
  }

  implicit val stringEncoder: Encoder[String] =
    instance(xml.Text.apply)

  implicit val intEncoder: Encoder[Int] =
    instance(t => xml.Text(t.toString))

  implicit val longEncoder: Encoder[Long] =
    instance(t => xml.Text(t.toString))

  implicit val floatEncoder: Encoder[Float] =
    instance(t => xml.Text(t.toString))

  implicit val doubleEncoder: Encoder[Double] =
    instance(t => xml.Text(t.toString))

  implicit val booleanEncoder: Encoder[Boolean] =
    instance(t => xml.Text(t.toString))

  given optionEncoder[T](using tx: Encoder[T]): Encoder[Option[T]] =
    new Encoder[Option[T]] {
      def toXml(t: Option[T]): NodeSeq =
        t.fold(NodeSeq.Empty)(tx.toXml)

      override def toXml(t: Option[T], rootLabel: String, format: TagFormat): NodeSeq = 
        t match {
          case Some(x) => tx.toXml(x, rootLabel, format)
          case None    => XML loadString s"<$rootLabel/>"
        }
    }
  
  given listEncoder[T](using tx: Encoder[T]): Encoder[List[T]] =
    new Encoder[List[T]] {
      def toXml(t: List[T]): NodeSeq =
        t.foldLeft(NodeSeq.Empty)(_ ++ tx.toXml(_))

      override def toXml(t: List[T], rootLabel: String, format: TagFormat): NodeSeq =
        Elem(null, format.formatCase(rootLabel), Null, TopScope, true, toXml(t): _*)
    }

  given mutableSeqEncoder[T](using tx: Encoder[T]): Encoder[Seq[T]] =
    new Encoder[Seq[T]] {
      def toXml(t: Seq[T]): NodeSeq =
        t.foldLeft(NodeSeq.Empty)(_ ++ tx.toXml(_))

      override def toXml(t: Seq[T], rootLabel: String, format: TagFormat): NodeSeq =
        Elem(null, format.formatCase(rootLabel), Null, TopScope, true, toXml(t): _*)
    }
}