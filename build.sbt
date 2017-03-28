
lazy val root = (project in file(".")).
  settings(
    name := "EPS",
    version := "1.0",
    scalaVersion := "2.11.8",
    scalacOptions := Seq("-unchecked", "-deprecation", "-feature"),
    libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "junit" % "junit" % "4.12" % "test")
  ).dependsOn(fuel,swim)


lazy val fuel = RootProject( file("../fuel") )
lazy val swim = RootProject( file("../swim") )
