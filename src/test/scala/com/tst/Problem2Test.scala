package com.tst

import com.tst.SubsetGenerator.{allCombinablePromotions, combinablePromotions}
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.language.postfixOps

class Problem2Test extends org.scalatest.funsuite.AnyFunSuite {

  val promotions = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2"))) // P5 is not combinable with P2

  test("Find Best Group Rate real test") {
    val subsets = allCombinablePromotions(promotions)

    subsets should contain theSameElementsAs List(PromotionCombo(List("P3", "P4", "P5")), PromotionCombo(List("P1", "P4", "P5")), PromotionCombo(List("P2", "P3")), PromotionCombo(List("P1", "P2")))
  }

  test("Find Best Group Rate for a code real test") {
    val subsets = combinablePromotions("P3", promotions)

    subsets should contain theSameElementsAs List(PromotionCombo(List("P3", "P4", "P5")), PromotionCombo(List("P2", "P3")))
  }


}
