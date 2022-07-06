import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

val scala_212 = "2.12.15"
val scala_213 = "2.13.8"

val current_version = scala_213

val V = new {
  val cats                   = "2.7.0"
  val `cats-effect`          = "3.3.12"
  val `cats-discipline`      = "1.5.1"
  val `discipline-munit`     = "1.0.9"
  val circe                  = "0.14.1"
  val fs2                    = "3.2.7"
  val jedis                  = "3.2.0"
  val `kind-projector`       = "0.13.2"
  val kittens                = "2.3.2"
  val `log-effect`           = "0.16.3"
  val logback                = "1.2.3"
  val munit                  = "0.7.29"
  val `parallel-collections` = "1.0.4"
  val redis4Cats             = "1.0.0-RC3"
  val refined                = "0.10.1"
  val scalacheck             = "1.16.0"
  val `scala-redis`          = "3.30"
  val `scodec-bits`          = "1.1.31"
  val `scodec-core`          = "1.11.9"
  val `scodec-stream`        = "3.0.2"
  val scredis                = "2.3.3"
  val shapeless              = "2.3.9"
  val zio                    = "1.0.0"
  val `zio-interop-cats`     = "2.1.4.0"
}

val `cats-core`          = Def.setting("org.typelevel" %% "cats-core" % V.cats)
val `cats-effect-kernel` = Def.setting("org.typelevel" %% "cats-effect-kernel" % V.`cats-effect`)
val `cats-effect-std`    = Def.setting("org.typelevel" %% "cats-effect-std" % V.`cats-effect`)
val `cats-effect`        = Def.setting("org.typelevel" %% "cats-effect" % V.`cats-effect`)
val `cats-laws`          = Def.setting("org.typelevel" %% "cats-laws" % V.cats)
val `circe-core`         = Def.setting("io.circe" %%% "circe-core" % V.circe)
val `circe-parser`       = Def.setting("io.circe" %%% "circe-parser" % V.circe)
val `fs2-core`           = Def.setting("co.fs2" %%% "fs2-core" % V.fs2)
val `fs2-io`             = Def.setting("co.fs2" %% "fs2-io" % V.fs2)
val jedis                = Def.setting("redis.clients" % "jedis" % V.jedis)
val kittens              = Def.setting("org.typelevel" %%% "kittens" % V.kittens)
val `log-effect-fs2`     = Def.setting("io.laserdisc" %%% "log-effect-fs2" % V.`log-effect`)
val `log-effect-zio`     = Def.setting("io.laserdisc" %% "log-effect-zio" % V.`log-effect`)
val logback              = Def.setting("ch.qos.logback" % "logback-classic" % V.logback)
val redis4Cats           = Def.setting("dev.profunktor" %% "redis4cats-effects" % V.redis4Cats)
val refined              = Def.setting("eu.timepit" %%% "refined" % V.refined)
val `scala-redis`        = Def.setting("net.debasishg" %% "redisclient" % V.`scala-redis`)
val `scodec-bits`        = Def.setting("org.scodec" %%% "scodec-bits" % V.`scodec-bits`)
val `scodec-core`        = Def.setting("org.scodec" %%% "scodec-core" % V.`scodec-core`)
val `scodec-stream`      = Def.setting("org.scodec" %%% "scodec-stream" % V.`scodec-stream`)
val scredis              = Def.setting("com.github.scredis" %% "scredis" % V.scredis)
val shapeless            = Def.setting("com.chuusai" %%% "shapeless" % V.shapeless)
val zio                  = Def.setting("dev.zio" %% "zio" % V.zio)
val `zio-interop-cats`   = Def.setting("dev.zio" %% "zio-interop-cats" % V.`zio-interop-cats`)

val `cats-discipline`    = Def.setting("org.typelevel" %% "discipline-core" % V.`cats-discipline` % Test)
val `discipline-munit`   = Def.setting("org.typelevel" %% "discipline-munit" % V.`discipline-munit` % Test)
val `circe-generic`      = Def.setting("io.circe" %%% "circe-generic" % V.circe % Test)
val `refined-scalacheck` = Def.setting("eu.timepit" %%% "refined-scalacheck" % V.refined % Test)
val scalacheck           = Def.setting("org.scalacheck" %%% "scalacheck" % V.scalacheck % Test)
val munit                = Def.setting("org.scalameta" %%% "munit" % V.munit % Test)
val `munit-scalacheck`   = Def.setting("org.scalameta" %%% "munit-scalacheck" % V.munit % Test)

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
    munit,
    `munit-scalacheck`
  )
}

val coreLawsDeps = Def.Initialize.join {
  Seq(
    `cats-core`,
    `cats-laws`,
    `cats-discipline`,
    `discipline-munit`
  )
}

val fs2Deps = Def.Initialize
  .join {
    Seq(
      `cats-core`,
      `kind-projector-compiler-plugin`,
      `cats-effect`,
      `fs2-core`,
      `fs2-io`,
      kittens,
      `log-effect-fs2`,
      `scodec-stream`,
      scalacheck,
      munit,
      `munit-scalacheck`
    )
  }
  .zipWith(`scala-parallel-collections`) {
    case (ms, None)    => ms
    case (ms, Some(m)) => ms :+ m
  }

val fs2BenchDeps = Def.Initialize.join {
  Seq(
    `kind-projector-compiler-plugin`,
    jedis,
    redis4Cats,
    `scala-redis`,
    scredis,
    zio,
    `zio-interop-cats`,
    `log-effect-zio`,
    logback
  )
}

val circeDeps = Def.Initialize.join {
  Seq(
    `circe-core`,
    `circe-parser`,
    `circe-generic`,
    scalacheck,
    munit,
    `munit-scalacheck`
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
      case Some((2, major)) if major >= 13 =>
        flags ++ Seq(
          "-Wconf:any:error",
          "-Ypatmat-exhaust-depth",
          "off"
        )
      case _ =>
        flags ++ Seq(
          "-Xfuture",                         // Turn on future language features.
          "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
          "-Xlint:unsound-match",             // Pattern match may not be typesafe.
          "-Yno-adapted-args",       // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
          "-Ypartial-unification",   // Enable partial unification in type constructor inference
          "-Ywarn-inaccessible",     // Warn about inaccessible types in method signatures.
          "-Ywarn-infer-any",        // Warn when a type argument is inferred to be `Any`.
          "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
          "-Ywarn-nullary-unit"      // Warn when nullary methods return Unit.
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

lazy val commonSettings = Seq(
  scalaVersion       := current_version,
  crossScalaVersions := Seq(scala_212, scala_213),
  scalacOptions ++= versionDependantScalacOptions.value,
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings", "-Wconf:any:error"),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value
)

lazy val publishSettings = Seq(
  Test / publishArtifact := false,
  pomIncludeRepository   := (_ => false),
  organization           := "io.laserdisc",
  homepage               := Some(url("http://laserdisc.io")),
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
  licenses := Seq("MIT" -> url("https://raw.githubusercontent.com/laserdisc-io/laserdisc/master/LICENSE"))
)

lazy val testSettings = Seq(
  testFrameworks += new TestFramework("munit.Framework"),
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

lazy val scaladocSettings = Seq(
  Compile / doc / scalacOptions ++= Seq(
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    (LocalRootProject / baseDirectory).value.getAbsolutePath,
    "-implicits",
    "-implicits-show-all"
  ),
  Compile / doc / scalacOptions -= "-Xfatal-warnings",
  apiMappings ++= externalApiMappings.value
)

lazy val scoverageSettings = Seq(
  coverageMinimumStmtTotal := 60,
  coverageFailOnMinimum    := false,
  coverageHighlighting     := true
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
    Test / boilerplateSource    := baseDirectory.value.getParentFile / "src" / "test" / "boilerplate"
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

lazy val laws = project
  .in(file("laws"))
  .dependsOn(core.jvm)
  .settings(commonSettings ++ testSettings)
  .settings(
    name := "laws",
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
    name            := "laserdisc-core-benchmarks",
    scalaVersion    := current_version,
    publishArtifact := false
  )

lazy val `fs2-bench` = project
  .in(file("benchmarks/fs2"))
  .dependsOn(fs2)
  .enablePlugins(JmhPlugin)
  .settings(
    name            := "laserdisc-fs2-benchmarks",
    scalaVersion    := current_version,
    publishArtifact := false,
    scalacOptions ++= versionDependantScalacOptions.value,
    libraryDependencies ++= fs2BenchDeps.value,
    run / fork := true
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
  .aggregate(core.jvm, core.js, laws, fs2, cli, circe.jvm, circe.js)
  .settings(publishSettings)
  .settings(
    scalaVersion    := current_version,
    publishArtifact := false,
    addCommandAlias("benchClean", "all core-bench/clean fs2-bench/clean"),
    addCommandAlias("benchCompile", "all core-bench/compile fs2-bench/compile"),
    addCommandAlias("benchBuild", "benchClean; benchCompile"),
    addCommandAlias("fmt", "all scalafmtAll scalafmtSbt core-bench/scalafmtAll fs2-bench/scalafmtAll"),
    addCommandAlias("fmtCheck", "all scalafmtCheckAll scalafmtSbtCheck core-bench/scalafmtCheckAll fs2-bench/scalafmtCheckAll"),
    addCommandAlias("fullTest", "clean; coverage; test; coverageReport"),
    addCommandAlias("prePr", "fmtCheck; fullTest"),
    addCommandAlias(
      "setReleaseOptions",
      """set scalacOptions ++= Seq("-opt:l:method", "-opt:l:inline", "-opt-inline-from:laserdisc.**", "-opt-inline-from:<sources>")"""
    ),
    addCommandAlias("releaseIt", "clean; setReleaseOptions; session list; compile; ci-release")
  )

Global / excludeLintKeys += scalaJSLinkerConfig
