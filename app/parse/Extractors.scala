package parse

import scala.xml.NodeSeq
import model._


private[parse] case class SizeFragment(cascadeDefault: NodeSeq) extends Extractor[Size] {

  val size = cascadeDefault \\ "size"
  val sizeArray: Array[Int] = size.text.split(" ").map(_.toInt)

  override def extract(): Size = sizeArray match {
    case Array(width, height) => Size(width, height)
  }
}

private[parse] case class RectFragment(parentFragment: NodeSeq) extends Extractor[Seq[Rect]] {
  val rects = parentFragment \ "_"

  override def extract(): Seq[Rect] = rects.map(_.text.split(" ").map(_.toDouble)).map { case Array(x1, x2, y1, y2, w) => Rect(x1, x2, y1, y2, w) }
}

private[parse] case class FeatureFragment(parentFragment: NodeSeq) extends Extractor[Feature] {

  val feature = parentFragment \\ "feature"

  override def extract(): Feature = {
    val rects = RectFragment(feature \\ "rects")
    val tilted = (feature \\ "tilted").text match { case "0" => false case "1" => true}

    Feature(rects.extract(), tilted)
  }
}

private[parse] case class TreeFragment(parentFragment: NodeSeq) extends Extractor[Tree] {

  val feature = FeatureFragment(parentFragment).extract()
  val extractDouble = (name:String) => (parentFragment \\ name).text.toDouble
  val threshold = extractDouble("threshold")
  val left = extractDouble("left_val")
  val right = extractDouble("right_val")

  override def extract(): Tree = Tree(feature, threshold, left, right)
}

private[parse] case class TreeListFragment(parentFragment: NodeSeq) extends Extractor[Seq[Tree]] {
  val trees = parentFragment \ "_"

  override def extract(): Seq[Tree] = trees.map(TreeFragment(_).extract())
}

private[parse] case class StageFragment(parentFragment: NodeSeq) extends Extractor[Stage] {
  val trees = TreeListFragment(parentFragment \ "trees").extract()
  val threshold = (parentFragment \\ "stage_threshold").text.toDouble
  val parent = parentFragment \\ "parent"
  val next = parentFragment \\ "next"

  override def extract(): Stage = Stage(trees, threshold)
}

private[parse] case class StageListFragment(parentFragment: NodeSeq) extends Extractor[Seq[Stage]] {
  val stages = parentFragment \ "_"

  override def extract(): Seq[Stage] = stages.map(StageFragment(_).extract())
}

private[parse] case class HarrClassifierFragment(parentFragment: NodeSeq) extends Extractor[HaarClassifier] {
  val stages = StageListFragment(parentFragment \ "stages").extract()
  val size = SizeFragment(parentFragment).extract()
  override def extract(): HaarClassifier = HaarClassifier(stages, size)
}
