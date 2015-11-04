scalaVersion := "2.11.7"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.0" % "test"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.4" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.4" // MutableInt for chapter5lazycheck

scalacOptions ++= Seq(
  "-deprecation",           
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",                
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
//  "-Xfatal-warnings",       
  "-Xlint",
  "-Yno-adapted-args",       
  "-Ywarn-dead-code",        // N.B. doesn't work well with the ??? hole
  "-Ywarn-numeric-widen",   
  "-Ywarn-value-discard",
  "-Xfuture"
  ,"-Xexperimental"
)

scalacOptions in compile += "-Ywarn-unused-import"     // 2.11 only
