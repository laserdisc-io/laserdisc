// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val V = new {
  val cats                   = "2.1.0"
  val `cats-discipline`      = "1.0.2"
  val `discipline-scalatest` = "1.0.0"
  val circe                  = "0.12.3"
  val fs2                    = "2.2.1"
  val `kind-projector`       = "0.11.0"
  val kittens                = "2.0.0"
  val `log-effect-fs2`       = "0.12.1"
  val `parallel-collections` = "0.2.0"
  val refined                = "0.9.12"
  val scalacheck             = "1.14.3"
  val scalatest              = "3.0.8"
  val `scodec-bits`          = "1.1.12"
  val `scodec-core`          = "1.11.4"
  val `scodec-stream`        = "2.0.0"
  val shapeless              = "2.3.3"
}

val `cats-core`      = Def.setting("org.typelevel" %% "cats-core"       % V.cats)
val `cats-laws`      = Def.setting("org.typelevel" %% "cats-laws"       % V.cats)
val `circe-core`     = Def.setting("io.circe"      %%% "circe-core"     % V.circe)
val `circe-parser`   = Def.setting("io.circe"      %%% "circe-parser"   % V.circe)
val `fs2-core`       = Def.setting("co.fs2"        %%% "fs2-core"       % V.fs2)
val `fs2-io`         = Def.setting("co.fs2"        %% "fs2-io"          % V.fs2)
val kittens          = Def.setting("org.typelevel" %%% "kittens"        % V.kittens)
val `log-effect-fs2` = Def.setting("io.laserdisc"  %%% "log-effect-fs2" % V.`log-effect-fs2`)
val refined          = Def.setting("eu.timepit"    %%% "refined"        % V.refined)
val `scodec-bits`    = Def.setting("org.scodec"    %%% "scodec-bits"    % V.`scodec-bits`)
val `scodec-core`    = Def.setting("org.scodec"    %%% "scodec-core"    % V.`scodec-core`)
val `scodec-stream`  = Def.setting("org.scodec"    %%% "scodec-stream"  % V.`scodec-stream`)
val shapeless        = Def.setting("com.chuusai"   %%% "shapeless"      % V.shapeless)

val `cats-discipline`      = Def.setting("org.typelevel"  %% "discipline-core"      % V.`cats-discipline`      % Test)
val `discipline-scalatest` = Def.setting("org.typelevel"  %% "discipline-scalatest" % V.`discipline-scalatest` % Test)
val `circe-generic`        = Def.setting("io.circe"       %%% "circe-generic"       % V.circe                  % Test)
val `refined-scalacheck`   = Def.setting("eu.timepit"     %%% "refined-scalacheck"  % V.refined                % Test)
val scalacheck             = Def.setting("org.scalacheck" %%% "scalacheck"          % V.scalacheck             % Test)
val scalatest              = Def.setting("org.scalatest"  %%% "scalatest"           % V.scalatest              % Test)

val `scala-parallel-collections` = Def.setting {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 13 =>
      Some("org.scala-lang.modules" %%% "scala-parallel-collections" % V.`parallel-collections` % Test)
    case _ =>
      None
  }
}

val `kind-projector-compiler-plugin` = Def.setting {
  compilerPlugin("org.typelevel" % "kind-projector" % V.`kind-projector` cross CrossVersion.full)
}

val coreDeps = Def.Initialize.join {
  Seq(
    `kind-projector-compiler-plugin`,
    refined,
    `scodec-bits`,
    `scodec-core`,
    shapeless,
    `refined-scalacheck`,
    scalacheck,
    scalatest
  )
}

val coreLawsDeps = Def.Initialize.join {
  Seq(
    `cats-core`,
    `cats-laws`,
    `cats-discipline`,
    `discipline-scalatest`
  )
}

val fs2Deps = Def.Initialize
  .join {
    Seq(
      `kind-projector-compiler-plugin`,
      `fs2-core`,
      `fs2-io`,
      kittens,
      `log-effect-fs2`,
      `scodec-stream`,
      scalacheck,
      scalatest
    )
  }
  .zipWith(`scala-parallel-collections`) {
    case (ms, None)    => ms
    case (ms, Some(m)) => ms :+ m
  }

val circeDeps = Def.Initialize.join {
  Seq(
    `circe-core`,
    `circe-parser`,
    `circe-generic`,
    scalacheck,
    scalatest
  )
}

val externalApiMappings = Def.task {
  val fullClassPath = (Compile / fullClasspath).value

  sealed trait DocumentationSite {
    def maybeDocsFor(module: ModuleID): Option[(File, URL)]
    protected final def maybeModuleFile(module: ModuleID): Option[File] =
      fullClassPath
        .find {
          _.get(moduleID.key).exists { m =>
            m.organization == module.organization && m.name.startsWith(module.name) && m.revision == module.revision
          }
        }
        .map(_.data)
  }

  object JavaDocIo extends DocumentationSite {
    override final def maybeDocsFor(m: ModuleID): Option[(File, URL)] = {
      val (organization, version) = m.organization -> m.revision
      val crossVersionedName = CrossVersion(m.crossVersion, scalaVersion.value, scalaBinaryVersion.value)
        .fold(m.name)(_.apply(m.name))
      maybeModuleFile(m)
        .map(_ -> url(s"https://static.javadoc.io/$organization/$crossVersionedName/$version"))
    }
  }

  (coreDeps.value ++ fs2Deps.value ++ circeDeps.value :+ (scalaOrganization.value % "scala-library" % scalaVersion.value))
    .flatMap(JavaDocIo.maybeDocsFor)
    .toMap
}

val versionDependantScalacOptions = Def.setting {
  def versionDependent(scalaVersion: String, flags: Seq[String]) =
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, major)) if major >= 13 => flags
      case _ =>
        flags ++ Seq(
          "-Xfuture",                         // Turn on future language features.
          "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
          "-Xlint:unsound-match",             // Pattern match may not be typesafe.
          "-Yno-adapted-args",                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
          "-Ypartial-unification",            // Enable partial unification in type constructor inference
          "-Ywarn-inaccessible",              // Warn about inaccessible types in method signatures.
          "-Ywarn-infer-any",                 // Warn when a type argument is inferred to be `Any`.
          "-Ywarn-nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Ywarn-nullary-unit"               // Warn when nullary methods return Unit.
        )
    }

  val flags = Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8",                         // Specify character encoding used by source files.
    "-explaintypes",                 // Explain type errors in more detail.
    "-feature",                      // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds",         // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings",              // Fail the compilation if there are any warnings.
    "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver.
    "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
    "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override",       // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",        // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
    "-Ywarn-dead-code",              // Warn when dead code is identified.
    "-Ywarn-numeric-widen",          // Warn when numerics are widened.
    "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
    "-Ywarn-extra-implicit",         // Warn when more than one implicit parameter section is defined.
    "-Ywarn-unused:explicits",       // Warn if a parameter is unused.
    "-Ywarn-unused:imports",         // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals",          // Warn if a local definition is unused.
    "-Ywarn-unused:patvars",         // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates",        // Warn if a private member is unused.
    "-Ywarn-value-discard"           // Warn when non-Unit expression results are unused.
  )

  versionDependent(scalaVersion.value, flags)
}

inThisBuild {
  organization := "io.laserdisc"
}

lazy val commonSettings = Seq(
  scalacOptions ++= versionDependantScalacOptions.value,
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := (_ => false),
  developers := List(
    Developer("sirocchi", "Julien Sirocchi", "julien.sirocchi@gmail.com", url("https://github.com/sirocchj")),
    Developer("barambani", "Filippo Mariotti", "", url("https://github.com/barambani"))
  ),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/laserdisc-io/laserdisc/tree/master"),
      "scm:git:git@github.com:laserdisc-io/laserdisc.git",
      "scm:git:git@github.com:laserdisc-io/laserdisc.git"
    )
  ),
  homepage := Some(url("http://laserdisc.io")),
  licenses := Seq("MIT" -> url("https://raw.githubusercontent.com/laserdisc-io/laserdisc/master/LICENSE")),
  pgpPublicRing := file(".travis/local.pubring.asc"),
  pgpSecretRing := file(".travis/local.secring.asc"),
  releaseEarlyWith := SonatypePublisher
)

lazy val testSettings = Seq(
  Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oDF")
)

lazy val scaladocSettings = Seq(
  Compile / doc / scalacOptions ++= Seq(
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-implicits",
    "-implicits-show-all"
  ),
  Compile / doc / scalacOptions -= "-Xfatal-warnings",
  apiMappings ++= externalApiMappings.value
)

lazy val scoverageSettings = Seq(
  coverageMinimum := 60,
  coverageFailOnMinimum := false,
  coverageHighlighting := true
)

lazy val allSettings = commonSettings ++ testSettings ++ scaladocSettings ++ publishSettings ++ scoverageSettings

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(allSettings)
  .settings(
    name := "laserdisc-core",
    libraryDependencies ++= coreDeps.value,
    Compile / boilerplateSource := baseDirectory.value.getParentFile / "src" / "main" / "boilerplate",
    Test / boilerplateSource := baseDirectory.value.getParentFile / "src" / "test" / "boilerplate"
  )
  .jsSettings(
    coverageEnabled := false
  )
  .jvmSettings(
    javaOptions += "-Djava.net.preferIPv4Stack=true",
    Test / fork := true,
    initialCommands := s"""
      |import laserdisc._
      |import laserdisc.auto._
      |import laserdisc.all._
      |import shapeless._
      |""".stripMargin
  )
  .jsConfigure(_.enablePlugins(BoilerplatePlugin))
  .jvmConfigure(_.enablePlugins(BoilerplatePlugin))

lazy val `core-laws` = project
  .in(file("core-laws"))
  .dependsOn(core.jvm)
  .settings(commonSettings ++ testSettings)
  .settings(
    name := "laserdisc-core-laws",
    libraryDependencies ++= coreDeps.value ++ coreLawsDeps.value,
    publishArtifact := false
  )

lazy val fs2 = project
  .in(file("fs2"))
  .dependsOn(core.jvm)
  .settings(allSettings)
  .settings(
    name := "laserdisc-fs2",
    libraryDependencies ++= fs2Deps.value
  )

lazy val `core-bench` = project
  .in(file("benchmarks/core"))
  .dependsOn(core.jvm % "compile->test;compile->compile")
  .enablePlugins(JmhPlugin)
  .settings(
    name := "laserdisc-core-benchmarks",
    publishArtifact := false
  )

lazy val cli = project
  .in(file("cli"))
  .dependsOn(fs2)
  .settings(allSettings)
  .settings(
    name := "laserdisc-cli",
    libraryDependencies ++= fs2Deps.value
  )

lazy val circe = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("circe"))
  .dependsOn(core)
  .settings(allSettings)
  .settings(
    name := "laserdisc-circe",
    libraryDependencies ++= circeDeps.value
  )
  .jsSettings(
    coverageEnabled := false
  )

lazy val laserdisc = project
  .in(file("."))
  .aggregate(core.jvm, core.js, `core-laws`, fs2, cli, circe.jvm, circe.js)
  .settings(publishSettings)
  .settings(
    publishArtifact := false,
    addCommandAlias("fmt", ";scalafmt;test:scalafmt;scalafmtSbt"),
    addCommandAlias("fmtCheck", ";scalafmtCheck;test:scalafmtCheck;scalafmtSbtCheck"),
    addCommandAlias("fullBuild", ";fmtCheck;clean;coverage;test;coverageReport")
  )
