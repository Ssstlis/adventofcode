package io.github.weakteam

class Day4Spec extends BaseStaticSpec with Load {
  val dayNumber = "4"

  "Day4#exampleRun" should {
    "run well" in {
      loadExample.map(Day4.simpleRun) mustBe Some(4512)
    }
  }

  "Day4#simpleRun" should {
    "run well" in {
      load.map(Day4.simpleRun) mustBe Some(69579)
    }
  }

  "Day4#exampleDifficultRun" should {
    "run well" in {
      loadExample.map(Day4.difficultRun) mustBe Some(1924)
    }
  }

  "Day4#difficultRun" should {
    "run well" in {
      load.map(Day4.difficultRun) mustBe Some(14877)
    }
  }

}
