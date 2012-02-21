package sbt

/**
 * Sbt 0.11.0 is missing a readLine function with masking
 * This class provides this functionality for sbt 0.11.0
 */
object Sbt110Compatibility {

  trait WithMaskingReadline {
    def readLine(prompt: String, mask: Option[Char] = None): Option[String]
  }

  implicit def withReadLine(reader: sbt.JLine): WithMaskingReadline = new WithMaskingReadline {
    val readerField = {
      val f = reader.getClass.getDeclaredField("reader")
      f.setAccessible(true)
      f
    }

    def jlineReader = readerField.get(reader).asInstanceOf[jline.ConsoleReader]

    def readLine(prompt: String, mask: Option[Char] = None) = sbt.JLine.withJLine { unsynchronizedReadLine(prompt, mask) }
    private[this] def unsynchronizedReadLine(prompt: String, mask: Option[Char]) =
      (mask match {
        case Some(m) => jlineReader.readLine(prompt, m)
        case None => jlineReader.readLine(prompt)
      }) match
      {
        case null => None
        case x => Some(x.trim)
      }
  }
}
