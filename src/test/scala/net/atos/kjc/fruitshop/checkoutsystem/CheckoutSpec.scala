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

    "return proper price output for list of correct items equal to £ 2.55" in {
      Checkout.processor(listOfValidItems) shouldBe "[apple, orange] = £ 2.55"
    }
    "return proper price output equal to double list of correct items equal to £5.10" in {
      Checkout.processor(listOf2xValidItems) shouldBe "[apple, orange] = £ 5.10"
    }
    "return `Nothing to process`" in {
      Checkout.processor(Nil) shouldBe "Nothing to process"
    }
    "return `Their is no apples or oranges`" in {
      Checkout.processor(listOfInvalidItems) shouldBe "Their is no apples or oranges"
    }
    "return proper price for mics list equal to £2.55" in {
      Checkout.processor(micsListOfItems) shouldBe "[apple, orange] = £ 2.55"
    }
  }
}
