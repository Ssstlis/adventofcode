package io.github.weakteam

class Day1Spec extends BaseStaticSpec with Load {
  val dayNumber = "1"

  "Day1#exampleRun" should {
    "run well" in {
      loadExample.map(Day1.simpleRun) mustBe Some(7)
    }
  }

  "Day1#simpleRun" should {
    "run well" in {
      load.map(Day1.simpleRun) mustBe Some(1266)
    }
  }

  "Day1#exampleDifficultRun" should {
    "run well" in {
      loadExample.map(Day1.difficultRun) mustBe Some(5)
    }
  }

  "Day1#difficultRun" should {
    "run well" in {
      load.map(Day1.difficultRun) mustBe Some(1217)
    }
  }

}
