val defaultSettings = List(
  scalaVersion := "2.13.7",
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.7.0",
    "org.scalatest" %% "scalatest" % "3.2.10" % Test
  )
)
val utils = project.in(file("utils")).settings(defaultSettings)

val aoc2021 = project
  .in(file("aoc2021"))
  .settings(defaultSettings)
  .dependsOn(utils)
  .aggregate(utils)
