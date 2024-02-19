package com.tst

import com.tst.FindBestGroupRate.{cabinPrices, getBestGroupPrice, rates}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Problem1Test extends org.scalatest.funsuite.AnyFunSuite {

  val expectedResult = Seq(
    BestGroupPrice("CA", "M1", 200.00, "Military"),
    BestGroupPrice("CA", "S1", 225.00, "Senior"),
    BestGroupPrice("CB", "M1", 230.00, "Military"),
    BestGroupPrice("CB", "S1", 245.00, "Senior")
  )

  test("getting min element") {
    getBestGroupPrice(expectedResult) shouldBe BestGroupPrice("CA", "M1", 200.00, "Military")
  }

  test("Find Best Group Rate") {
    FindBestGroupRate.getBestGroupPrices(rates, cabinPrices) shouldBe expectedResult
  }
}
