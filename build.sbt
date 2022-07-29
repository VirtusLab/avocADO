val scala3 = "3.2.0-RC1"

lazy val avocado = project
  .in(file("."))
  .settings(
    name := "avocADO",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3,

    scalacOptions ++= Seq(
      "-Xcheck-macros"
      // "-Xprint:typer"
    ),

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "pprint" % "0.7.3",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
