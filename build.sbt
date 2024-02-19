lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.tst",
      scalaVersion := "2.13.12"
    )),
    name := "travel-test"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test