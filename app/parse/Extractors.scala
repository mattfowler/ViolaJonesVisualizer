package parse

import scala.xml.NodeSeq
import model.{Tree, Feature, Rect, Size, Stage}


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

case class FeatureFragment(parentFragment: NodeSeq) extends Extractor[Feature] {

  val feature = parentFragment \\ "feature"

  override def extract(): Feature = {
    val rects = RectFragment(feature \\ "rects")
    val tilted = (feature \\ "tilted").text match { case "0" => false case "1" => true}

    Feature(rects.extract(), tilted)
  }
}

case class TreeFragment(parentFragment: NodeSeq) extends Extractor[Tree] {

  val feature = FeatureFragment(parentFragment).extract()
  val extractDouble = (name:String) => (parentFragment \\ name).text.toDouble
  val threshold = extractDouble("threshold")
  val left = extractDouble("left_val")
  val right = extractDouble("right_val")

  override def extract(): Tree = Tree(feature, threshold, left, right)
}

case class TreeListFragment(parentFragment: NodeSeq) extends Extractor[Seq[Tree]] {
  val trees = parentFragment \ "_"

  override def extract(): Seq[Tree] = trees.map(TreeFragment(_).extract())
}

case class StageFragment(parentFragment: NodeSeq) extends Extractor[Stage] {
  val trees = TreeListFragment(parentFragment \ "trees").extract()
  val threshold = (parentFragment \\ "stage_threshold").text.toDouble
  val parent = parentFragment \\ "parent"
  val next = parentFragment \\ "next"

  override def extract(): Stage = Stage(trees, threshold)
}
