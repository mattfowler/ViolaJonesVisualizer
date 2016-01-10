package parse

import model._

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
    val xml =
      <rects>
        <_>1 2 3 4 5.</_>
      </rects>
    RectFragment(xml).extract() should be(Seq(Rect(1.0, 2.0, 3.0, 4.0, 5.0)))
  }

  it should "be able to extract mutiple rects" in {
    val xml =
      <rects>
        <_>1 2 3 4 5.</_> <_>1 1 2 2 -5.</_>
      </rects>

    RectFragment(xml).extract() should be(Seq(Rect(1.0, 2.0, 3.0, 4.0, 5.0), Rect(1.0, 1.0, 2.0, 2.0, -5.0)))
  }

  behavior of "Feature parsing"

  it should "be able to extract a feature with a few rects" in {
    val xml =
      <_>
        <feature>
          <rects>
            <_>3 9 18 9 -1.</_>
            <_>3 12 18 3 3.</_>
          </rects>
          <tilted>0</tilted>
        </feature>
      </_>

    FeatureFragment(xml).extract() should be(Feature(Seq(Rect(3.0, 9.0, 18.0, 9.0, -1.0), Rect(3.0, 12.0, 18.0, 3.0, 3.0)), false))
  }

  behavior of "Tree parsing"

  it should "be able to extract a tree with a feature and threshold" in {
    val xml =
      <_>
        <feature>
          <rects>
            <_>3 9 18 9 -1.</_>
            <_>3 12 18 3 3.</_>
          </rects>
          <tilted>0</tilted>
        </feature>
        <threshold>0.0102530000731349</threshold>
        <left_val>-0.6085060238838196</left_val>
        <right_val>0.7709850072860718</right_val>
      </_>

    val expectedFeature = Feature(Seq(Rect(3.0, 9.0, 18.0, 9.0, -1.0), Rect(3.0, 12.0, 18.0, 3.0, 3.0)), false)
    TreeFragment(xml).extract() should be(Tree(expectedFeature, 0.0102530000731349, -0.6085060238838196, 0.7709850072860718))
  }

  it should "be able to extract a list of trees" in {
    val xml =
      <trees>
        <_>
          <!-- tree 0 -->
          <_>
            <!-- root node -->
            <feature>
              <rects>
                <_>3 9 18 9 -1.</_>
                <_>3 12 18 3 3.</_>
              </rects>
              <tilted>0</tilted>
            </feature>
            <threshold>-0.0315119996666908</threshold>
            <left_val>2.0875380039215088</left_val>
            <right_val>-2.2172100543975830</right_val>
          </_>
        </_>
        <_>
          <!-- tree 1 -->
          <_>
            <!-- root node -->
            <feature>
              <rects>
                <_>3 9 18 9 -1.</_>
                <_>3 999 18 3 3.</_>
              </rects>
              <tilted>1</tilted>
            </feature>
            <threshold>0.0123960003256798</threshold>
            <left_val>-1.8633940219879150</left_val>
            <right_val>1.3272049427032471</right_val>
          </_>
        </_>
      </trees>

    val expectedFeature1 = Feature(Seq(Rect(3.0, 9.0, 18.0, 9.0, -1.0), Rect(3.0, 12.0, 18.0, 3.0, 3.0)), false)
    val expectedFeature2 = Feature(Seq(Rect(3.0, 9.0, 18.0, 9.0, -1.0), Rect(3.0, 999.0, 18.0, 3.0, 3.0)), true)
    val expectedTree1 = Tree(expectedFeature1, -0.0315119996666908, 2.0875380039215088, -2.2172100543975830)
    val expectedTree2 = Tree(expectedFeature2, 0.0123960003256798, -1.8633940219879150, 1.3272049427032471)

    val extracted: Seq[Tree] = TreeListFragment(xml).extract()
    extracted.head should be(expectedTree1)
    extracted.last should be(expectedTree2)
  }

  behavior of "Stage parsing"

  it should "be able to parse a single stage" in {
    val xml =
      <_>
        <trees>
          <_>
            <!-- tree 0 -->
            <_>
              <!-- root node -->
              <feature>
                <rects>
                  <_>3 9 18 9 -1.</_>
                  <_>3 12 18 3 3.</_>
                </rects>
                <tilted>0</tilted>
              </feature>
              <threshold>-0.0315119996666908</threshold>
              <left_val>2.0875380039215088</left_val>
              <right_val>-2.2172100543975830</right_val>
            </_>
          </_>
          <_>
            <!-- tree 1 -->
            <_>
              <!-- root node -->
              <feature>
                <rects>
                  <_>3 9 18 9 -1.</_>
                  <_>3 999 18 3 3.</_>
                </rects>
                <tilted>1</tilted>
              </feature>
              <threshold>0.0123960003256798</threshold>
              <left_val>-1.8633940219879150</left_val>
              <right_val>1.3272049427032471</right_val>
            </_>
          </_>
        </trees>
        <stage_threshold>1.123</stage_threshold>
        <parent>-1</parent>
        <next>-1</next>
      </_>

    val expectedFeature1 = Feature(Seq(Rect(3.0, 9.0, 18.0, 9.0, -1.0), Rect(3.0, 12.0, 18.0, 3.0, 3.0)), false)
    val expectedFeature2 = Feature(Seq(Rect(3.0, 9.0, 18.0, 9.0, -1.0), Rect(3.0, 999.0, 18.0, 3.0, 3.0)), true)
    val expectedTree1 = Tree(expectedFeature1, -0.0315119996666908, 2.0875380039215088, -2.2172100543975830)
    val expectedTree2 = Tree(expectedFeature2, 0.0123960003256798, -1.8633940219879150, 1.3272049427032471)

    val extracted: Stage = StageFragment(xml).extract()
    extracted.trees should be (Seq(expectedTree1, expectedTree2))
    extracted.stageThreshold should be (1.123)
  }

}
