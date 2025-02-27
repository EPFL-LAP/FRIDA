val scala3 = "3.3.3"
val scala2 = "2.13.12"
val chiselVersion = "6.2.0"

Global / excludeLintKeys += traceLevel

// Needed because of the gurobi native library loads...
(Global / run / fork) := true
(Global / outputStrategy) := Some(StdoutOutput)

(Global / connectInput) := true

lazy val dot2blif = (project in file("dot2blif"))
  .settings (
    name := "dot2blif",
    traceLevel := 20,
    scalaVersion := scala3,
    libraryDependencies ++= Seq(
      ("com.gurobi" % "gurobi" % "11.0.1"),
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"),
      ("org.scala-lang.modules" %% "scala-xml" % "2.0.1").cross(CrossVersion.for3Use2_13), // To make hw happy
    ),
    scalacOptions ++= Seq("-deprecation"), // "-explain"
  )

ThisBuild / assemblyMergeStrategy := {
  case PathList("module-info.class")               => MergeStrategy.last
  case path if path.endsWith("/mod3ule-info.class") => MergeStrategy.last
  case PathList("META-INF", xs @ _*)               => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

lazy val hw = (project in file("hardware_components"))
  .settings (
    name := "hw",
    assembly / mainClass := Some("components.Main"),
    scalaVersion := scala2,
    libraryDependencies ++= Seq (
      ("com.gurobi" % "gurobi" % "11.0.1"),
      "org.chipsalliance" %% "chisel" % chiselVersion
      //"edu.berkeley.cs" %% "chiseltest" % chiselVersion % "test"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations",
      "-Ytasty-reader"
    ),
    addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full)
  ).dependsOn(dot2blif)


lazy val root = (project in file("."))
  .aggregate(dot2blif, hw)
