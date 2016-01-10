package parse

import controllers.SizeFragment
import model.Size

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


}
