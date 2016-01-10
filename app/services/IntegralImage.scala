package services

case class ArrayCoordinate(x: Int, y: Int)

case class IntegralImage(image: Array[Array[Int]]) {

  private val zeroes: Array[Int] = Array.fill[Int](image(0).length)(0)

  private val integralImage = image.scanLeft(zeroes)((sum, current) => (sum, current.scanLeft(0)(_ + _).tail).zipped.map(_ + _)).tail

  def valueAt(row: Int, column: Int): Double = integralImage(row)(column)

  def sumOfRectangle(upperLeftHandCorner: ArrayCoordinate, width: Int, height: Int): Int = {
    lazy val upperLeft = integralImage(upperLeftHandCorner.y - 1)(upperLeftHandCorner.x - 1)
    lazy val lowerLeft = integralImage(upperLeftHandCorner.y + height)(upperLeftHandCorner.x - 1)
    lazy val upperRight = integralImage(upperLeftHandCorner.y - 1)(upperLeftHandCorner.x + width)
    lazy val lowerRight = integralImage(upperLeftHandCorner.y + height)(upperLeftHandCorner.x + width)

    upperLeftHandCorner match {
      case ArrayCoordinate(0, 0) => lowerRight
      case ArrayCoordinate(0, y) if y > 0 => lowerRight - upperRight
      case ArrayCoordinate(x, 0) if x > 0 => lowerRight - lowerLeft
      case ArrayCoordinate(x, y) => lowerRight - upperRight - lowerLeft + upperLeft
    }
  }

}
