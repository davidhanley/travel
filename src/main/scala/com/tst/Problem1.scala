package com.tst

case class Rate(rateCode: String,
                rateGroup: String)


case class CabinPrice(cabinCode: String,
                      rateCode: String,
                      price: BigDecimal)


case class BestGroupPrice(cabinCode: String,
                          rateCode: String,
                          price: BigDecimal,
                          rateGroup: String)

object BestGroupPrice {
  def apply(rate: Map[String, String], cabinPrice: CabinPrice): Option[BestGroupPrice] = {
    val rateGroup: Option[String] = rate.get(cabinPrice.rateCode)
    rateGroup.map { rate =>
      BestGroupPrice(
        cabinCode = cabinPrice.cabinCode,
        rateCode = cabinPrice.rateCode,
        price = cabinPrice.price,
        rateGroup = rate
      )
    }
  }
}

object FindBestGroupRate {
  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    val rateCodeToGroup: Map[String, String] = rates.map(r => (r.rateCode, r.rateGroup)).toMap

    val groupPrices: Seq[BestGroupPrice] = prices.flatMap(cp => BestGroupPrice(rateCodeToGroup, cp))

    val bestGroupPrices =
      groupPrices.groupBy(bgp => bgp.rateGroup + bgp.cabinCode).values.map(_.sortBy(_.price)).map(_.head)

    bestGroupPrices.toSeq.sortBy(_.rateCode).sortBy(_.cabinCode)
  }
}

