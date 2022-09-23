val scala3 = "3.2.0"

val commonSettings = Seq(
  organization := "org.virtuslab",
  description := "Safe compile-time parallelization of for-comprehensions for Scala 3",
  homepage := Some(url("https://github.com/VirtusLab/avocADO")),
  licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  developers := List(
    Developer(
      "KacperFKorban",
      "Kacper Korban",
      "kacper.f.korban@gmail.com",
      url("https://twitter.com/KacperKorban")
    )
  ),
  scalacOptions ++= Seq(
    "-Xcheck-macros",
    "-Ycheck:inlining",
    "-explain",
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "0.7.29" % Test
  )
)

lazy val root = project
  .in(file("."))
  .settings(commonSettings)
  .settings(
    name := "avocADO-root",
    publish / skip := true
  )
  .aggregate((
    avocado.projectRefs
      ++ catsEffect3.projectRefs
      ++ zio2.projectRefs
      ++ zio1.projectRefs
  )*)

lazy val avocado = projectMatrix
  .in(file("avocADO"))
  .settings(commonSettings)
  .settings(
    name := "avocADO"
  )
  .jvmPlatform(scalaVersions = List(scala3))

lazy val catsEffect3 = projectMatrix
  .in(file("cats-effect-3"))
  .settings(commonSettings)
  .settings(
    name := "avocADO-cats-effect-3",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % "3.3.14"
    )
  )
  .dependsOn(avocado)
  .jvmPlatform(scalaVersions = List(scala3))

lazy val zio2 = projectMatrix
  .in(file("zio-2"))
  .settings(commonSettings)
  .settings(
    name := "avocADO-zio-2",
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % "2.0.0"
    )
  )
  .dependsOn(avocado)
  .jvmPlatform(scalaVersions = List(scala3))

lazy val zio1 = projectMatrix
  .in(file("zio-1"))
  .settings(commonSettings)
  .settings(
    name := "avocADO-zio-1",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "1.0.17"
    )
  )
  .dependsOn(avocado)
  .jvmPlatform(scalaVersions = List(scala3))
