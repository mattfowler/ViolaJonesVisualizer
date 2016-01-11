package parse

import java.net.URL

import model.Size
import org.scalatest.{FlatSpec, Matchers}


class HaarClassifierParser$Test extends FlatSpec with Matchers {

  val testFile: URL = getClass.getResource("/test_classifier.xml")

  it should "parse a haar classifier" in {
    val classifier = HaarClassifierParser.fromFile(testFile)

    classifier.size should be(Size(24, 24))
    classifier.stages.size should be(2)
  }
}
