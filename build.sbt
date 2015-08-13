scalaVersion := "2.11.7"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.4" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")
