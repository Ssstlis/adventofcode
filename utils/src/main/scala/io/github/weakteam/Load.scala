package io.github.weakteam

import io.github.weakteam.util._

trait Load {
  def dayNumber: String

  private val loader = getClass.getClassLoader

  private val __load = (s: String) =>
    Option(loader.getResource(s))
      .map(_.getFile)
      .flatMap(readFileLines(_).toOption)

  def loadExample = __load(s"day$dayNumber.example.txt")

  def load = __load(s"day$dayNumber.txt")
}
