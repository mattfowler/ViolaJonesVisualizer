package model


case class Size(width: Int, size: Int) {
  require(width > 0 && size > 0)
}

case class HaarClassifier(stages: Array[Stage], size: Size)

case class Stage(trees: Array[Tree], stageThreshold: Double)

case class Tree(feature: Feature, threshold: Double, leftValue: Double, rightValue: Double)

case class Feature(featureRectangles: Array[Rect])

case class Rect(x1: Double, x2: Double, y1: Double, y2: Double, weight: Double)

