import sbt.Keys.{libraryDependencies, scalaVersion}

enablePlugins(JmhPlugin)


lazy val commonSettings = Seq(
	scalaVersion := "2.12.8",
	javacOptions ++= Seq(
		"-target", "1.8",
		"-source", "1.8",
		"-Xlint:deprecation")
)

lazy val scalacLintAll = Seq(
	scalacOptions ++= Seq(
		"-target:jvm-1.8",
		"-encoding", "UTF-8",
		"-unchecked",
		"-deprecation",
		"-explaintypes",
		"-feature",
		"-Xfuture",

		"-language:existentials",
		"-language:experimental.macros",
		"-language:higherKinds",
		"-language:postfixOps",
		"-language:implicitConversions",

		"-Xlint:adapted-args",
		"-Xlint:by-name-right-associative",
		"-Xlint:constant",
		"-Xlint:delayedinit-select",
		"-Xlint:doc-detached",
		"-Xlint:inaccessible",
		"-Xlint:infer-any",
		"-Xlint:missing-interpolator",
		"-Xlint:nullary-override",
		"-Xlint:nullary-unit",
		"-Xlint:option-implicit",
		"-Xlint:package-object-classes", // too widespread
		"-Xlint:poly-implicit-overload",
		"-Xlint:private-shadow",
		"-Xlint:stars-align",
		"-Xlint:type-parameter-shadow",
		"-Xlint:unsound-match",

		"-Yno-adapted-args",
		"-Ywarn-dead-code",
		"-Ywarn-extra-implicit",
		"-Ywarn-inaccessible",
		"-Ywarn-infer-any",
		"-Ywarn-nullary-override",
		"-Ywarn-nullary-unit",
		"-Ywarn-numeric-widen",
		"-Ywarn-unused:implicits",
		//		"-Ywarn-unused:imports",
		"-Ywarn-unused:locals",
		"-Ywarn-unused:params",
		"-Ywarn-unused:patvars",
		"-Ywarn-unused:privates",
		"-Ywarn-value-discard",
		"-Ypartial-unification",
	),
)

lazy val JmhVersion = "1.21"

lazy val localProjectSettings = Seq(
	organization := "net.kurobako",
	version := "0.1.0-SNAPSHOT",
) ++ commonSettings ++ scalacLintAll


lazy val TestDependencies = Seq(
	"org.scalatest" %% "scalatest" % "3.0.5" % Test,
	"org.scalacheck" %% "scalacheck" % "1.14.0" % Test
)
lazy val JmhDependencies = Seq(
	"org.openjdk.jmh" % "jmh-core" % JmhVersion,
	"org.openjdk.jmh" % "jmh-generator-annprocess" % JmhVersion % Compile,
)

lazy val fixtures = project.settings(
	localProjectSettings,
	libraryDependencies ++= JmhDependencies
)

lazy val BenchDependencies = TestDependencies ++ JmhDependencies

lazy val `jvm-bench` = project.settings(
	localProjectSettings,
	libraryDependencies ++= BenchDependencies ++ Seq("org.typelevel" %% "cats-core" % "1.6.0"),
).dependsOn(fixtures).enablePlugins(JmhPlugin)
 

lazy val AllBenches = Seq(
	`jvm-bench`,
)

val benchmarkAll = taskKey[String]("Run all individual benchmarks")

def mkJmhArgs(iteration: Int = 10,
			  warmup: Int = 10,
			  fork: Int = 1,
			  thread: Int = 1,
			  format: String = "json",
			  filename: String) = Seq("i" -> iteration,
	"wi" -> warmup,
	"f" -> fork,
	"t" -> thread,
	"rf" -> format,
	"rff" -> filename)
	.map { case (flag, v) => s"-$flag $v" }.mkString(" ")


lazy val `scala-collection-benchmarks` = (project in file(".")).settings(
	name := "scala-collection-benchmarks",
	localProjectSettings,
	libraryDependencies ++= BenchDependencies,
	cancelable in Global := true,
	benchmarkAll := Def.taskDyn {
		Def.sequential(
			clean +:
			AllBenches.map(project => Def.sequential(((Jmh / run) in project)
				.toTask(" " + mkJmhArgs(
					//					iteration = 5,
					//					warmup = 5,
					filename = s"../docs/jmh_${project.id}.json")))),
			Def.task("Done"))
	}.value,
)
	.aggregate(AllBenches.map(Project.projectToLocalProject(_)): _*)
	.enablePlugins(JmhPlugin)


