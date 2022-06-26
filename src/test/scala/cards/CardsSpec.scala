package cards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cards.domain.Card

class CardsSpec extends AnyFlatSpec with Matchers {
  behavior of "The cards nominals"
  "The 2c " should "have nominal 0" in {
    Card(0).nominal shouldEqual 0
  }
  it should "have suit 0" in {
    Card(0).suit shouldEqual 0
  }
  "The 2d " should "have nominal 0" in {
    Card(13).nominal shouldEqual 0
  }
  it should "have suit 1" in {
    Card(13).suit shouldEqual 1
  }
  "The 2h " should "have nominal 0" in {
    Card(26).nominal shouldEqual 0
  }
  it should "have suit 2" in {
    Card(26).suit shouldEqual 2
  }
  "The Jh " should "have nominal 9" in {
    Card(35).nominal shouldEqual 9
  }
  it should "have suit 2" in {
    Card(35).suit shouldEqual 2
  }
  "The As " should "have nominal 12" in {
    Card(51).nominal shouldEqual 12
  }
  it should "have suit 3" in {
    Card(51).suit shouldEqual 3
  }
}