package model

import services.{ArrayCoordinate, IntegralImage}


case class Size(width: Int, size: Int) {
  require(width > 0 && size > 0)
}

case class HaarClassifier(stages: Seq[Stage], size: Size)

case class Stage(trees: Seq[Tree], stageThreshold: Double)

case class Tree(feature: Feature, threshold: Double, leftValue: Double, rightValue: Double)

case class Feature(featureRectangles: Seq[Rect], tilted: Boolean)

case class Rect(x1: Double, x2: Double, y1: Double, y2: Double, weight: Double) {

  def sumForImage(image: IntegralImage, coordinates: ArrayCoordinate, scale: Double): Double = {
    val width = (x2 - x1) * scale
    val height = (y2 - y1) * scale
    image.sumOfRectangle(coordinates, width.toInt, height.toInt) * weight
  }
}

