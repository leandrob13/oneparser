val scala3Version = "3.0.1"

lazy val `oneparser-xml` = project
  .settings(
    name := "oneparser-xml",
    version := "0.1.0",
    scalaVersion := "3.0.1",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-xml" % "2.0.1",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )

lazy val oneparser = (project in file(".")).aggregate(`oneparser-xml`)
