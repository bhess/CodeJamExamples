name := "CodeJamExamples"

version := "1.0"

showSuccess := false

mainClass in (Compile, run) := Some("ItzChess")

libraryDependencies ++= Seq(
  "org.scala-saddle" %% "saddle-core" % "1.3.+"
  // (OPTIONAL) "org.scala-saddle" %% "saddle-hdf5" % "1.3.+"
)