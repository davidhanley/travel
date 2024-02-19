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
  // construct a BestGroupPrice option from a cabin price and a rate map.
  // if the rate doesn't exist, use "None" -- at first i returned a None and used
  // flatmap later, but i think you *would* want to show the codes on the user interface,
  // so i changed that
  def apply(rate: Map[String, String], cabinPrice: CabinPrice): BestGroupPrice = {
    BestGroupPrice(
      cabinCode = cabinPrice.cabinCode,
      rateCode = cabinPrice.rateCode,
      price = cabinPrice.price,
      rateGroup = rate.get(cabinPrice.rateCode).getOrElse("None"))
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

  def getBestGroupPrice(groupPrices: Seq[BestGroupPrice]): BestGroupPrice = {
    groupPrices.minBy(_.price) //at first i sorted and took the head, this is obviously better
  }

  def getBestGroupPrices(rates: Seq[Rate],
                         prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    //it simplifies the code to have this as a map, and helps eliminate worst cases of N^2 times
    val rateCodeToGroup: Map[String, String] = rates.map(rate => rate.rateCode -> rate.rateGroup).toMap

    //create the group prices
    val groupPrices: Seq[BestGroupPrice] = prices.map(cabinPrice => BestGroupPrice(rateCodeToGroup, cabinPrice))

    //group the prices by rateGroup and cabinCode, then choose the lowest groupPrice from each of those 
    val bestGroupPrices =
      groupPrices.groupBy(bestGroupPrice => Seq(bestGroupPrice.rateGroup, bestGroupPrice.cabinCode)).values.map(getBestGroupPrice)

    //sort to the order shown in sample data, cabin code major sort, rate code minor.  Will help agents find the codes quickly.
    //Since scala's sort is stable you can sort by the minor field, then the major,
    bestGroupPrices.toSeq.sortBy(_.rateCode).sortBy(_.cabinCode)
  }

  def main(args: Array[String]): Unit = {
    for (rate <- FindBestGroupRate.getBestGroupPrices(rates, cabinPrices)) {
      println(rate)
    }
  }
}

