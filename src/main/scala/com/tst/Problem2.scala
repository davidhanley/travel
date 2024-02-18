package com.tst

case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String])

object SubsetGenerator {

  def removeContainedSets(sets: Set[Set[String]]): Set[Set[String]] = {
    sets.filter { set =>
      !sets.exists(otherSet => set != otherSet && set.subsetOf(otherSet))
    }
  }

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {

    val exclusionRules: Map[String, Set[String]] = allPromotions.map(p => (p.code -> p.notCombinableWith.toSet)).toMap

    val searchSpace = exclusionRules.keys.toSeq

    def recursiveHelper(searchSpace: Seq[String], acc: Set[String]): Set[Set[String]] = {
      searchSpace match {
        case Seq() => if (acc.size > 1) Set(acc) else Set.empty
        case first :: rest =>
          exclusionRules.get(first) match {
            case None => recursiveHelper(rest, acc + first)
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
  }


  def combinablePromotions(
                            promotionCode: String,
                            allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allPromotions.find(_.code == promotionCode) match {
      case None => allCombinablePromotions(allPromotions) // code isn't in there, just return all promotions
      case Some(promo) =>
        val exclusions = promo.notCombinableWith.toSet
        val filtered = allPromotions.filterNot(promo => exclusions.contains(promo.code))
        allCombinablePromotions(filtered)
    }
  }

  def main(args: Array[String]): Unit = {

  }
}
