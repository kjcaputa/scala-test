package net.atos.kjc.fruitshop.checkoutsystem

import org.scalatest.{FreeSpec, Matchers}

class CheckoutSpec extends FreeSpec with Matchers {
  val listOfValidItems: List[String] = List("Apple", "apple", "APPLE", "Orange", "orange", "ORANGE")
  val listOfInvalidItems: List[String] = List("OrangeOrange", "AnotherOrange", "AppleApple", "AnotherApple", "differentFruit")
  val listOf2xValidItems: List[String] = listOfValidItems ::: listOfValidItems
  val micsListOfItems: List[String] = listOfValidItems ::: listOfInvalidItems

  "Checkout system should" - {
    "prices for a unit should" - {
      "for an apple equal 0.60" in {
        Checkout.prices.get("apple").get shouldBe 0.60 +- 0.001
      }
      "for an orange equal 0.25" in {
        Checkout.prices.get("orange").get shouldBe 0.25 +- 0.001
      }
      "for unknown product is None" in {
        Checkout.prices.get("unknown") shouldBe None
      }
    }

    "test of empty list should return `Nothing to process`" in {
      Checkout.processor(Nil) shouldBe "Nothing to process"
    }
    "test of invalid list of products should return `Their is no apples or oranges`" in {
      Checkout.processor(listOfInvalidItems) shouldBe "Their is no apples or oranges"
    }
    "test with the valid items list should show `3x apple` and `3x orange` and total cost equal to £ 2.55" in {
      Checkout.processor(listOfValidItems) shouldBe "[3x apple, 3x orange] = £ 2.55"
    }
    "test with twice longer the valid items list should show `6x apple` and `6x orange` and total cost equal to £ 5.10" in {
      Checkout.processor(listOf2xValidItems) shouldBe "[6x apple, 6x orange] = £ 5.10"
    }
    "test with mix of the valid items list  and the invalid item list should show `3x apple` and `3x orange` and total cost equal to £ 2.55" in {
      Checkout.processor(micsListOfItems) shouldBe "[3x apple, 3x orange] = £ 2.55"
    }
  }
}
