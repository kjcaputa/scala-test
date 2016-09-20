package net.atos.kjc.fruitshop.checkoutsystem

object Checkout {
  val prices: Map[String, Double] = Map("apple" -> 0.60, "orange" -> 0.25)

  type FilteredProducts = Map[String, Int]

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
      filter.foldLeft((List[String](), 0.0d))((acc, entry) =>
        (acc._1 :+ s"${entry._2}x ${entry._1}", acc._2 + entry._2 * prices.getOrElse(entry._1, 0.0d))
      )

    if (listOfItems.isEmpty)
      "Nothing to process"
    else if (filter.keys.isEmpty)
      "Their is no apples or oranges"
    else {
      "[%s] = £ %2.2f".format(calculateTotal._1.mkString(", "), calculateTotal._2)
    }
  }
}
