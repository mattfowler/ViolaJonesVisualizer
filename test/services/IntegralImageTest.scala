package services

import org.scalatest._

class IntegralImageTest extends FlatSpec with Matchers  {

  it should "be able to sum a one row matrix" in {
    val input = Array(Array(1, 1, 0, 0, 0, 1, 1))

    val integralImg = IntegralImage(input)

    integralImg.valueAt(0, 0) should be(1)
    integralImg.valueAt(0, 1) should be(2)
    integralImg.valueAt(0, 2) should be(2)

    integralImg.valueAt(0, 5) should be(3)
    integralImg.valueAt(0, 6) should be(4)
  }

  it should "be able to sum multi row matrices" in {
    val input = Array(Array(1, 1, 0, 0, 0, 1, 1),
                      Array(0, 0, 1, 0, 1, 1, 1))

    val integralImg = IntegralImage(input)

    integralImg.valueAt(1, 0) should be (1)
    integralImg.valueAt(1, 3) should be (3)
    integralImg.valueAt(1, 6) should be (8)

    val threeRows = Array(Array(1, 1, 0, 0, 1, 1),
                          Array(0, 0, 1, 1, 0, 0),
                          Array(1, 0, 0, 0, 0, 1))

    val threeRowImage = IntegralImage(threeRows)

    threeRowImage.valueAt(2, 5) should be (8)
  }

  it should "be able to compute the sum of a rectangle" in {

    val threeRows = Array(Array(1, 1, 0, 0, 1, 1),
                          Array(0, 0, 1, 1, 0, 0),
                          Array(1, 0, 0, 0, 0, 1))

    val threeRowImage = IntegralImage(threeRows)

    threeRowImage.sumOfRectangle(ArrayCoordinate(4, 1), 1, 1) should be (1)
    threeRowImage.sumOfRectangle(ArrayCoordinate(2, 1), 3, 1) should be (3)
    threeRowImage.sumOfRectangle(ArrayCoordinate(0, 0), 1, 1) should be (2)
    threeRowImage.sumOfRectangle(ArrayCoordinate(0, 1), 1, 1) should be (1)
    threeRowImage.sumOfRectangle(ArrayCoordinate(2, 0), 2, 2) should be (3)
  }

}
