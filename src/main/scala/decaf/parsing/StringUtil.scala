package decaf.parsing

object StringUtil {
  def quote(str: String): String = {
    val sb = new StringBuilder
    str.foreach {
      case '"' => sb ++= "\\\""
      case '\n' => sb ++= "\\n"
      case '\t' => sb ++= "\\t"
      case '\\' => sb ++= "\\\\"
      case c => sb += c
    }
    "\"" + sb.toString + "\""
  }
}
