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

  test("make sure move element to head works") {
    val pc = PromotionCombo(Seq("A", "B", "C"))
    pc.movePromotionCodeToHead("C").promotionCodes should contain theSameElementsInOrderAs Seq("C", "A", "B")
  }

  test("Find Best Group Rate real test") {
    val subsets = allCombinablePromotions(promotions)

    subsets should contain theSameElementsInOrderAs
      List(
        PromotionCombo(List("P1", "P2")),
        PromotionCombo(List("P1", "P4", "P5")),
        PromotionCombo(List("P2", "P3")),
        PromotionCombo(List("P3", "P4", "P5"))
      )
  }

  test("Find Best Group Rate for P3 real test") {
    val subsets = combinablePromotions("P3", promotions)

    subsets should contain theSameElementsInOrderAs
      List(
        PromotionCombo(List("P3", "P2")),
        PromotionCombo(List("P3", "P4", "P5"))
      )
  }

  test("Find Best Group Rate for P1 real test") {
    val subsets = combinablePromotions("P1", promotions)

    subsets should contain theSameElementsAs
      List(
        PromotionCombo(List("P1", "P4", "P5")),
        PromotionCombo(List("P1", "P2")))
  }

  test("test the case with no exclusions") {
    val promotionsWithoutExclusions = Seq(
      Promotion("P1", Seq.empty),
      Promotion("P2", Seq.empty),
      Promotion("P3", Seq.empty),
      Promotion("P4", Seq.empty))

    val subsets = allCombinablePromotions(promotionsWithoutExclusions)

    subsets should contain theSameElementsAs List(PromotionCombo(List("P1", "P2", "P3", "P4")))
  }

  test("test the case with all exclusions") {
    val promotionsWithoutExclusions = Seq(
      Promotion("P1", Seq("P1", "P2", "P3", "P4")),
      Promotion("P2", Seq("P1", "P2", "P3", "P4")),
      Promotion("P3", Seq("P1", "P2", "P3", "P4")),
      Promotion("P4", Seq("P1", "P2", "P3", "P4")))

    val subsets = allCombinablePromotions(promotionsWithoutExclusions)

    //no combinations
    subsets should contain theSameElementsAs List.empty
  }

  test("test the case with two disjoint sets") {
    val promotionsWithoutExclusions = Seq(
      Promotion("P1", Seq("P3", "P4")),
      Promotion("P2", Seq("P3", "P4")),
      Promotion("P3", Seq("P1", "P2")),
      Promotion("P4", Seq("P1", "P2")))

    val subsets = allCombinablePromotions(promotionsWithoutExclusions)

    //no combinations
    subsets should contain theSameElementsAs List(PromotionCombo(List("P1", "P2")), PromotionCombo(List("P3", "P4")))
  }

}