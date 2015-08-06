name := "bootcamp"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.0.0",
  "pl.metastack" %%  "metarx" % "0.1.0",
  "com.lihaoyi" %% "scalarx" % "0.2.8",
  "org.specs2" %% "specs2" % "2.4.16" % "test"
)

transitiveClassifiers in Global := Seq(Artifact.SourceClassifier)
