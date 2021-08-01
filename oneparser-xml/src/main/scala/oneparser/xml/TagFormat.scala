package oneparser.xml

sealed trait TagFormat {
  def formatCase[T](t: T): String = {
    val (first, rest) = t match {
      case s: String => s.splitAt(1)
      case _ => t.getClass.getSimpleName.splitAt(1)  
    }
    tag(first, rest)
  }

  def tag(first: String, rest: String): String
}

object TagFormat {

  case object LowerCammelCase extends TagFormat {
    def tag(first: String, rest: String): String =
      s"${first.toLowerCase}$rest"
  }

  case object UpperCammelCase extends TagFormat {
    def tag(first: String, rest: String): String =
      s"${first.toUpperCase}$rest"
  }
  
  case object DashCase extends TagFormat {
    def tag(first: String, rest: String): String = {
      rest.foldLeft(first.toLowerCase) {
        case (str, chr) if chr.isUpper => s"$str-${chr.toLower}"
        case (str, chr) =>s"$str$chr"
      }
    }
  }
}
