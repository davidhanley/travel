package com.tst

case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String]) {

  //When we look for combinable codes containing a specific code, we list the
  //searched for code first, and this method acomplishes that
  def movePromotionCodeToHead(element: String): PromotionCombo = {
    PromotionCombo(Seq(element) ++ promotionCodes.filterNot(_ == element))
  }
}

object PromotionCombo {
  // Define an ordering for PromotionCombo, as the output specified is in sorted order
  // we can simplify a bit, for example, We don't need to choose how to sort (a,b) before or
  // after (a,b,c) becauee accordingto the rules, (a,b) would be subsumed by (a,b,c) as the size
  // 3 group containes the size 2 group. So we can zip and compare pairs
  implicit val ordering: Ordering[PromotionCombo] = new Ordering[PromotionCombo] {
    override def compare(x: PromotionCombo, y: PromotionCombo): Int = {
      val pairs = x.promotionCodes.zip(y.promotionCodes)
      for ((s1,s2) <- pairs) {
        val cmp = s1.compareTo(s2)
        if (cmp != 0) return cmp
      }
      // If all elements are equal, return 0, but this should not happen
      0
    }
  }
}

/**
 * Find promotions codes that can be used together.  There are rules preventing some codes from being
 * use with other codes, which means that, for example, if we have (a,b,c,d) an b cannot be used with
 * (c or d) the combinations of codes that can be used are (a,b) and (a,c,d).
 * The outline of the algorithm is that (in the case above) we pop 'a' off the search space, and if A
 * has no exclusions with the remaining values we can recusively produce a list of the combinable promotions
 * for (b,c,d) with 'a' in the accumulator.
 * When we get to the (b,c,d) case, we pop B off and see it does exclude (c,d) so we descend one branch with the
 * search space minus the excluded with 'b' added to the accumulator.  The search space is then empty and (a,b) is
 * returned.  In the other branch, there are no more exclusions and the search space is exhausted with (a,c,d)
 * in the accumulator, so that is returned.
 *
 * The obvious solution this graph problem would be a DFS search, removing duplicates at the end, but the
 * search space in a pathological case could be N!, having that many sets in RAM before the ending dedupe.
 * This algo's complexity would be O(n) of the resulting combinable groups except for the subset removal
 * which is N^2, so the overall complexity is (combinable groups)^2 which is a lot less than N! . The
 * algo makes 1 or 2 recursions per element, but the search space is pruned when the dual recursion happens.
 *
 * I also note that the supplied exclusion rules are bidirectional, so if a excludes b, b also excludes a.
 * If this isn't an invariant, we'd need to produce the inverse exclusions and add that to the exclusion set,
 * which is easy.
 *
 */
object CombinableCodes {

  val promotions = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2"))) // P5 is not combinable with P2

  //the search does produce groups that are contained by larger groups, so we need to filter those out
  def removeContainedSets(sets: Set[Set[String]]): Set[Set[String]] = {
    sets.filter { set =>
      !sets.exists(otherSet => set != otherSet && set.subsetOf(otherSet))
    }
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    //the exclusion rules are faster and less code to access as a map, so let's make them into one
    val exclusionRules: Map[String, Set[String]] =
      allPromotions.map(p => p.code -> p.notCombinableWith.toSet).toMap

    //by changing the search space to just strings, we can use set.contains to prune the search space as we recurse
    val searchSpace = exclusionRules.keys.toSeq

    //recursively break sets into head::tail or ::tail, produce groups, then combine the results and filter
    //out the smaller sets.
    def recursiveHelper(searchSpace: Seq[String], acc: Set[String]): Set[Set[String]] = {
      searchSpace match {
        case Seq() => if (acc.size > 1) Set(acc) else Set.empty
        case first :: rest =>
          //need to fix ths, we need to see if the remaining has any exclusions, there is always a rule, even if empty
          exclusionRules.get(first) match {
            case None => recursiveHelper(rest, acc + first) //we need to
            case Some(exclusions) =>
              val combinedBranches =
                recursiveHelper(rest.filterNot(exclusions.contains), acc + first) ++
                  recursiveHelper(rest, acc)
              removeContainedSets(combinedBranches)
          }
      }
    }

    recursiveHelper(searchSpace, Set.empty)
      .toSeq
      .map(s => PromotionCombo(s.toSeq.sorted))
      .sorted
  }


  def combinablePromotions(
                            promotionCode: String,
                            allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val promotions = allPromotions.find(_.code == promotionCode) match {
      case None => allCombinablePromotions(allPromotions) // code isn't in there, just return all promotions
      case Some(promo) =>
        val exclusions = promo.notCombinableWith.toSet
        val filtered = allPromotions.filterNot(promo => exclusions.contains(promo.code))
        allCombinablePromotions(filtered)
    }

    promotions.map(_.movePromotionCodeToHead(promotionCode))
  }

  def main(args: Array[String]): Unit = {

    def show(promotionCombos: Seq[PromotionCombo]):Unit = {
      for (promotionCombo <- promotionCombos) {
        println(promotionCombo)
      }
    }

    println("all combinable promotions:")
    show(allCombinablePromotions(promotions))

    def showForCode(code:String) = {
      println()
      println(s"combinable promotions for $code:")
      show(combinablePromotions(code, promotions))
    }

    showForCode("P1")
    showForCode("P3")
  }
}
