package net.atos.kjc.fruitshop.checkoutsystem

object Checkout {
  type FilteredProducts = Map[String, Int]
  type Offers = Map[String, (Int, Int)]

  val prices: Map[String, Double] = Map("apple" -> 0.60, "orange" -> 0.25)

  val offers: Offers = Map("apple" -> (2, 1), "orange" -> (3, 2))

  def processor(listOfItems: List[String]): String = {

    lazy val filter: FilteredProducts = {
      def filterHelper(listOfScannedItems: List[String], acc: FilteredProducts = Map()): FilteredProducts = {
        val matchValidProducts = s"(${prices.keys.mkString("|")})".r

        def updatedAcc(item: String): Map[String, Int] = {
          acc.get(item) match {
            case None => acc + (item -> 1)
            case _ => acc.map(elem => if (elem._1 == item) (elem._1, elem._2 + 1) else (elem))
          }
        }

        if (listOfScannedItems.isEmpty) acc
        else
          listOfScannedItems.head.toLowerCase match {
            case matchValidProducts(item) => filterHelper(listOfScannedItems.tail, updatedAcc(item))
            case _ => filterHelper(listOfScannedItems.tail, acc)
          }
      }

      filterHelper(listOfItems)
    }

    lazy val calculateTotal: (List[String], Double) =
      filter.foldLeft((List[String](), 0.0d)) { (acc, entry) =>
        val (name, totalQty) = entry
        val (accName, accTotalQty) = acc
        val unitCost = prices.getOrElse(name, 0.0d)
        val totalCost = offers.get(name) match {
          case None => accTotalQty + totalQty * unitCost
          case Some((amount, priceInRelationToUnitCost)) =>
            val qtyNotIncludedInOffer = totalQty % amount
            accTotalQty + (unitCost * priceInRelationToUnitCost * (totalQty - qtyNotIncludedInOffer) / amount) + (unitCost * qtyNotIncludedInOffer)
        }

        (accName :+ s"${totalQty}x ${name}", totalCost)
      }

    if (listOfItems.isEmpty)
      "Nothing to process"
    else if (filter.keys.isEmpty)
      "Their is no apples or oranges"
    else {
      "[%s] = £ %2.2f".format(calculateTotal._1.mkString(", "), calculateTotal._2)
    }
  }
}
