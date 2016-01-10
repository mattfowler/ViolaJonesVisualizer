package parse

import scala.xml.NodeSeq
import model.{Rect, Size}


case class SizeFragment(cascadeDefault: NodeSeq) extends Extractor[Size] {

  val size = cascadeDefault \\ "size"
  val sizeArray: Array[Int] = size.text.split(" ").map(_.toInt)

  override def extract(): Size = sizeArray match {
    case Array(width, height) => Size(width, height)
  }
}

case class RectFragment(parentFragment: NodeSeq) extends Extractor[Seq[Rect]] {
  val rects = parentFragment \ "_"

  override def extract(): Seq[Rect] = rects.map(_.text.split(" ").map(_.toDouble)).map { case Array(x1, x2, y1, y2, w) => Rect(x1, x2, y1, y2, w) }
}
