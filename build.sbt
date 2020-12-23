import sbt.Keys.{libraryDependencies, scalaVersion}

enablePlugins(JmhPlugin)

lazy val commonSettings = Seq(
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3"),
  scalaVersion := "2.13.4",
  scalacOptions ++= Seq(
    "-Ymacro-annotations"
  ),
  scalacOptions ~= filterConsoleScalacOptions,
  resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.jcenterRepo
  )
)

lazy val JmhVersion = "1.27"

lazy val localProjectSettings = Seq(
  organization := "net.kurobako",
  version := "0.1.0-SNAPSHOT"
) ++ commonSettings

lazy val TestDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.2.3" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test,
  "org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0" % Test
)
lazy val JmhDependencies = Seq(
  "org.openjdk.jmh" % "jmh-core" % JmhVersion,
  "org.openjdk.jmh" % "jmh-generator-annprocess" % JmhVersion % Compile
)

lazy val fixtures = project.settings(
  localProjectSettings,
  libraryDependencies ++= JmhDependencies
)

lazy val BenchDependencies = TestDependencies ++ JmhDependencies

lazy val `jvm-bench` = project
  .settings(
    localProjectSettings,
    libraryDependencies ++= BenchDependencies ++ Seq("org.typelevel" %% "cats-core" % "2.3.1")
  )
  .dependsOn(fixtures)
  .enablePlugins(JmhPlugin)

lazy val AllBenches = Seq(
  `jvm-bench`
)

val benchmarkAll = taskKey[String]("Run all individual benchmarks")

def mkJmhArgs(
    iteration: Int = 10,
    warmup: Int = 10,
    fork: Int = 1,
    thread: Int = 1,
    format: String = "json",
    filename: String
) = Seq(
  "i" -> iteration,
  "wi" -> warmup,
  "f" -> fork,
  "t" -> thread,
  "rf" -> format,
  "rff" -> filename
)
  .map { case (flag, v) => s"-$flag $v" }
  .mkString(" ")

lazy val `scala-collection-benchmarks` = (project in file("."))
  .settings(
    name := "scala-collection-benchmarks",
    localProjectSettings,
    libraryDependencies ++= BenchDependencies,
    cancelable in Global := true,
    benchmarkAll := Def.taskDyn {
      Def.sequential(
        clean +:
          AllBenches.map(project =>
            Def.sequential(
              ((Jmh / run) in project)
                .toTask(
                  " " + mkJmhArgs(
                    iteration = 5,
                    warmup = 5,
                    filename = s"../docs/jmh_${project.id}.json"
                  )
                )
            )
          ),
        Def.task("Done")
      )
    }.value
  )
  .aggregate(AllBenches.map(Project.projectToLocalProject(_)): _*)
  .enablePlugins(JmhPlugin)
