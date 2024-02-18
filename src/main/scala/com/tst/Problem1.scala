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
    rateGroup match {
      case None =>
        println(s"There is no group for code ${cabinPrice.rateCode}")
        None
      case Some(rate) =>
        Some(BestGroupPrice(
          cabinCode = cabinPrice.cabinCode,
          rateCode = cabinPrice.rateCode,
          price = cabinPrice.price,
          rateGroup = rate))
    }
  }
}

object FindBestGroupRate {

  val rates = Seq(
    Rate("M1", "Military"),
    Rate("M2", "Military"),
    Rate("S1", "Senior"),
    Rate("S2", "Senior")
  )

  val cabinPrices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )

  def getBestGroupPrice(groupprices: Seq[BestGroupPrice]): BestGroupPrice = {
    groupprices.sortBy(_.price).head
  }

  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    val rateCodeToGroup: Map[String, String] = rates.map(r => r.rateCode -> r.rateGroup).toMap

    val groupPrices: Seq[BestGroupPrice] = prices.flatMap(cp => BestGroupPrice(rateCodeToGroup, cp))

    val bestGroupPrices =
      groupPrices.groupBy(bgp => Seq(bgp.rateGroup, bgp.cabinCode)).values.map(getBestGroupPrice)

    //sort of the order shown, cabin code major sort, rate code minor.  Will help agents find the codes quickly.
    //Since scala's sort is stable you cna sort by the minor field, then the major,
    bestGroupPrices.toSeq.sortBy(_.rateCode).sortBy(_.cabinCode)
  }

  def main( args: Array[String]):Unit = {
    for( rate <- FindBestGroupRate.getBestGroupPrices(rates, cabinPrices)) {
      println(rate)
    }
  }
}

