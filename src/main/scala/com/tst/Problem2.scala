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
  // after (a,b,c) because according to the rules, (a,b) would be subsumed by (a,b,c) as the size
  // 3 group contains the size 2 group. So we can zip and compare pairs
  implicit val ordering: Ordering[PromotionCombo] = new Ordering[PromotionCombo] {
    override def compare(x: PromotionCombo, y: PromotionCombo): Int = {
      val pairs = x.promotionCodes.zip(y.promotionCodes)
      for ((s1, s2) <- pairs) {
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
 * The worst case for DFS ( a lot of elements with few or no exclusions ) is the best case here: only one
 * recursive call per element.
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
        case head :: tail =>
          val rule: Set[String] = exclusionRules.getOrElse(head, Set.empty) // should never be empty
          val filtered: Seq[String] = tail.filterNot(rule.contains(_))
          val combinedBranches =
            recursiveHelper(filtered, acc + head) ++
              (if (filtered.size != tail.size) recursiveHelper(tail, acc) else Set.empty)
          // ^^ if the current decision element doesn't exclude any of the remaining elements,
          // no need to recurse down the "without a head" element
          removeContainedSets(combinedBranches)
      }
    }

    recursiveHelper(searchSpace, Set.empty)
      .toSeq
      .map(s => PromotionCombo(s.toSeq.sorted))
      .sorted
  }

  // The promotions that can be combined with a specific promotion.
  // You can simply apply the exclusion rule of the specified promotion to the entire set, and
  // compute all promo combos for that with the preceding function.  Simple, and much more efficient
  // than computing all sets and filtering out all ones that do not contain the specified element
  def combinablePromotions(promotionCode: String,
                           allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    val exclusions: Set[String] = allPromotions.find(_.code == promotionCode) match {
      case None => Set.empty
      case Some(promotion) => promotion.notCombinableWith.toSet
    }

    val filtered = allPromotions.filterNot(promo => exclusions.contains(promo.code))
    val combinedPromotions = allCombinablePromotions(filtered)

    combinedPromotions.map(_.movePromotionCodeToHead(promotionCode))
  }

  def main(args: Array[String]): Unit = {

    def show(promotionCombos: Seq[PromotionCombo]): Unit = {
      for (promotionCombo <- promotionCombos) {
        println(promotionCombo)
      }
    }

    println("all combinable promotions:")
    show(allCombinablePromotions(promotions))

    def showForCode(code: String) = {
      println()
      println(s"combinable promotions for $code:")
      show(combinablePromotions(code, promotions))
    }

    showForCode("P1")
    showForCode("P3")
  }
}
