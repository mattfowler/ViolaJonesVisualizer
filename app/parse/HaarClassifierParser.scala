package parse

import java.net.URL

import scala.xml.XML
import model.HaarClassifier

object HaarClassifierParser {

  def fromFile(filePath: URL): HaarClassifier = {
    val file = XML.load(filePath)

    val root = file \\ "opencv_storage" \\ "haarcascade_frontalface_default"

    HaarClassifierFragment(root).extract()
  }
}
