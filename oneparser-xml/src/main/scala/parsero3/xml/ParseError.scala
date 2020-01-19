package oneparser.xml

sealed abstract class ParseError(message: String, label: String) extends Throwable(message)

object ParseError {
  final case class EmptyElementError(message: String, label: String)
      extends ParseError(message, label)

  final case class ConvertingError(message: String, label: String)
      extends ParseError(message, label)

  final case class UncaughtError(message: String, label: String)
      extends ParseError(message, label)
}