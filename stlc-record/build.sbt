name := "records"

version := "0.1.0"

organization := "inkytonik"

// Scala compiler settings

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
    "-deprecation", "-feature", "-unchecked",
    "-Xlint:-stars-align,_", "-Xfatal-warnings"
)

// Interactive settings

logLevel := Level.Info

shellPrompt in ThisBuild := {
    state =>
        Project.extract(state).currentRef.project + " " + version.value +
            " " + scalaVersion.value + "> "
}

// Fork the runs and connect sbt's input and output to the forked process so
// that we are immune to version clashes with the JLine library used by sbt

fork in run := true

connectInput in run := true

outputStrategy in run := Some(StdoutOutput)

// Dependencies

libraryDependencies ++=
    Seq(
        "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0-SNAPSHOT",
        "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0-SNAPSHOT" % "test" classifier ("tests"),
        "org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0-SNAPSHOT",
        "org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0-SNAPSHOT" % "test" classifier ("tests"),
        "org.scalatest" %% "scalatest" % "3.0.5" % "test",
        "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
    )

resolvers ++= Seq (
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots")
)

// Rats! setup

ratsScalaRepetitionType := Some(VectorType)
ratsUseScalaOptions := true
ratsUseScalaPositions := true
ratsDefineASTClasses := true
ratsDefinePrettyPrinter := true
ratsUseDefaultComments := true
ratsUseKiama := 2
