name := "lab01"

version := "0.1"

scalaVersion := "3.3.1"

scalacOptions ++= Seq("-source:future", "-indent", "-rewrite")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.18"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

wartremoverErrors ++= Warts.allBut(Wart.Any, Wart.Equals, Wart.Nothing,
  Wart.Overloading, Wart.Recursion, Wart.StringPlusAny,
  Wart.ToString, Wart.TripleQuestionMark)