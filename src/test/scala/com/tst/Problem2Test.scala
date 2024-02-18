package com.tst

import com.tst.CombinableCodes.{allCombinablePromotions, combinablePromotions, promotions, removeContainedSets}
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.language.postfixOps

class Problem2Test extends org.scalatest.funsuite.AnyFunSuite {


  test("test remove contained sets") {
    removeContainedSets(Set(Set("a"), Set("a", "b"), Set("a", "b", "c"), Set("y", "z"))) shouldBe
      Set(Set("a", "b", "c"), Set("y", "z"))
  }

  test("Find Best Group Rate real test") {
    val subsets = allCombinablePromotions(promotions)

    subsets should contain theSameElementsAs
      List(
        PromotionCombo(List("P3", "P4", "P5")),
        PromotionCombo(List("P1", "P4", "P5")),
        PromotionCombo(List("P2", "P3")),
        PromotionCombo(List("P1", "P2")))
  }

  test("Find Best Group Rate for P3 real test") {
    val subsets = combinablePromotions("P3", promotions)

    subsets should contain theSameElementsAs
      List(
        PromotionCombo(List("P3", "P4", "P5")),
        PromotionCombo(List("P2", "P3")))
  }

}
