val scala3 = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "avocado-root"
  )
  .aggregate(avocado.projectRefs*)

lazy val avocado = projectMatrix
  .in(file("core"))
  .settings(
    name := "avocADO",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3,

    scalacOptions ++= Seq(
      "-Xcheck-macros"
    ),

    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "pprint" % "0.7.3",
      "org.scalameta" %%% "munit" % "0.7.29" % Test,
      "org.typelevel" %%% "cats-effect" % "3.3.14" % Test,
      "dev.zio" %%% "zio" % "2.0.0" % Test
    )
  )
  .jvmPlatform(scalaVersions = List(scala3))


