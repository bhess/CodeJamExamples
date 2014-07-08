name := "CodeJamExamples"

version := "1.0"

mainClass in (Compile, run) := Some("TheRepeater")

libraryDependencies ++= Seq(
  "org.scala-saddle" %% "saddle-core" % "1.3.+"
  // (OPTIONAL) "org.scala-saddle" %% "saddle-hdf5" % "1.3.+"
)