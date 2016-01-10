package parse

import model.{Rect, Size}

import org.scalatest.{FlatSpec, Matchers}


class ExtractorsTest extends FlatSpec with Matchers {


  behavior of "Size parsing"

  it should "be able to extract the size" in {
    val xml =
      <enclosingElem>
        <size>24 24</size>
      </enclosingElem>

    SizeFragment(xml).extract() should be(Size(24, 24))
  }

  behavior of "Rectangle parsing"

  it should "be able to extract a single rect" in {
    val xml = <rects><_>1 2 3 4 5.</_></rects>
    RectFragment(xml).extract() should be(Seq(Rect(1.0, 2.0, 3.0, 4.0, 5.0)))
  }

  it should "be able to extract mutiple rects" in {
    val xml = <rects><_>1 2 3 4 5.</_><_>1 1 2 2 -5.</_></rects>

    RectFragment(xml).extract() should be(Seq(Rect(1.0, 2.0, 3.0, 4.0, 5.0), Rect(1.0, 1.0, 2.0, 2.0, -5.0)))
  }


}
