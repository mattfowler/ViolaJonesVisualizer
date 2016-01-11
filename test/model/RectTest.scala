package model

import org.scalatest.{Matchers, FlatSpec}
import services.{ArrayCoordinate, IntegralImage}

class RectTest extends FlatSpec with Matchers {

  behavior of "rectangle sum calculation"

  it should "be the sum of the rectangle where weight and scale are one" in {
    val twoByTwo = Rect(0, 1, 0, 1, 1)
    val image: IntegralImage = IntegralImage(Array(Array(1, 1), Array(1, 1)))

    twoByTwo.sumForImage(image, ArrayCoordinate(0, 0), 1) should be (4)
  }

  it should "be the sum of the rectangle multiplied by its weight" in {
    val rectWeight = 5
    val twoByTwo = Rect(0, 1, 0, 1, rectWeight)
    val image: IntegralImage = IntegralImage(Array(Array(1, 1), Array(1, 1)))

    twoByTwo.sumForImage(image, ArrayCoordinate(0, 0), 1) should be (4 * rectWeight)
  }

  it should "expand the rectangle by the scale" in {
    val scale = 2
    val twoByTwo = Rect(0, 1, 0, 1, 1)
    val fourOnes = Array.fill(4)(1)
    val image: IntegralImage = IntegralImage(Array.fill(4)(fourOnes))

    twoByTwo.sumForImage(image, ArrayCoordinate(0, 0), scale) should be (9)
  }

  it should "work on an arbitrary point in the image" in {
    val twoByTwo = Rect(0, 1, 0, 1, 1)
    val fourOnes = Array.fill(4)(1)
    val image: IntegralImage = IntegralImage(Array.fill(4)(fourOnes))

    twoByTwo.sumForImage(image, ArrayCoordinate(1, 1), 2) should be (9)
  }
}
