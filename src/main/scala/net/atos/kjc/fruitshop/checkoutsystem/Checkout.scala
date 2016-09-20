package net.atos.kjc.fruitshop.checkoutsystem

object Checkout {
  val prices: Map[String,Double] = Map("apple" -> 0.60, "orange" -> 0.25)

  type FilteredProducts = Map[String, Int]

  def processor(listOfItems: List[String]): String =  {

    lazy val filter: FilteredProducts = {
      def filterHelper(listOfScannedItems:List[String], acc: FilteredProducts = Map()) : FilteredProducts = {
        val matchValidProducts = s"(${prices.keys.mkString("|")})".r

        if(listOfScannedItems.isEmpty) acc
        else
          listOfScannedItems.head.toLowerCase match {
            case matchValidProducts(item) => filterHelper(listOfScannedItems.tail, acc + (item -> 1))
            case _ => filterHelper(listOfScannedItems.tail, acc)
          }
      }

      filterHelper(listOfItems)
    }

    if(listOfItems.isEmpty)
      "Nothing to process"
    else if (filter.keys.isEmpty)
      "Their is no apples or oranges"
    else
      ""
  }
}
