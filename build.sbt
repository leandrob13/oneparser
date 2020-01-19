val dottyVersion = "0.21.0-RC1"

lazy val `oneparser-xml` = project
  .settings(
    name := "oneparser-xml",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies ++= Seq(
      ("org.scala-lang.modules" %% "scala-xml" % "1.2.0").withDottyCompat(scalaVersion.value),
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )

lazy val oneparser = (project in file(".")).aggregate(`oneparser-xml`)
