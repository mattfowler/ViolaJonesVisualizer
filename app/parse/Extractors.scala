package parse

import scala.xml.NodeSeq
import model.Size


class Extractors {

  trait Extractor[T] {
    def extract() : T
  }

  case class SizeFragment(cascadeDefault: NodeSeq) extends Extractor[Size] {

    val size = cascadeDefault \\ "size"
    val sizeArray:Array[Int] = size.text.split(" ").map(_.toInt)

    override def extract(): Size = sizeArray match { case Array(width, height) => Size(width, height) }
  }

}
