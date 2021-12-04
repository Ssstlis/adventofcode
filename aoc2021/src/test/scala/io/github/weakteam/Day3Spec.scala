package io.github.weakteam

class Day3Spec extends BaseStaticSpec with Load {
  val dayNumber = "3"

  "Day3#exampleRun" should {
    "run well" in {
      loadExample.map(Day3.simpleRun) mustBe Some(BigInt(198))
    }
  }

  "Day3#simpleRun" should {
    "run well" in {
      load.map(Day3.simpleRun) mustBe Some(BigInt(3895776))
    }
  }

  "Day3#exampleDifficultRun" should {
    "run well" in {
      loadExample.map(Day3.difficultRun) mustBe Some(900)
    }
  }

  "Day3#difficultRun" should {
    "run well" in {
      load.map(Day3.difficultRun) mustBe Some(1982495697)
    }
  }

}
