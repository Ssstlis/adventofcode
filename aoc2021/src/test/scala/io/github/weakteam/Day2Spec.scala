package io.github.weakteam

class Day2Spec extends BaseStaticSpec with Load {
  val dayNumber = "2"

  "Day2#exampleRun" should {
    "run well" in {
      loadExample.map(Day2.simpleRun) mustBe Some(150)
    }
  }

  "Day2#simpleRun" should {
    "run well" in {
      load.map(Day2.simpleRun) mustBe Some(1924923)
    }
  }

  "Day2#exampleDifficultRun" should {
    "run well" in {
      loadExample.map(Day2.difficultRun) mustBe Some(900)
    }
  }

  "Day2#difficultRun" should {
    "run well" in {
      load.map(Day2.difficultRun) mustBe Some(1982495697)
    }
  }

}
