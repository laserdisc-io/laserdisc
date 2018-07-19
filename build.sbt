import sbtcrossproject.{CrossType, crossProject}

val V = new {
  val cats              = "1.1.0"
  val fs2               = "0.10.5"
  val `kind-projector`  = "0.9.7"
  val kittens           = "1.1.0"
  val refined           = "0.8.7" //FIXME can't upgrade see https://gist.github.com/sirocchj/64a00a28f5cc5776140c776c7db4e2e3
  val scalacheck        = "1.13.5"
  val scalatest         = "3.0.5"
  val `scodec-bits`     = "1.1.5"
  val `scodec-core`     = "1.10.3"
  val `scodec-stream`   = "1.1.0"
  val shapeless         = "2.3.3"
  val `log-effect-fs2`  = "0.1.6"
}

val `fs2-core`        = Def.setting("co.fs2"          %%% "fs2-core"        % V.fs2)
val `fs2-io`          = Def.setting("co.fs2"          %% "fs2-io"           % V.fs2)
val kittens           = Def.setting("org.typelevel"   %%% "kittens"         % V.kittens)
val refined           = Def.setting("eu.timepit"      %%% "refined"         % V.refined)
val `scodec-bits`     = Def.setting("org.scodec"      %%% "scodec-bits"     % V.`scodec-bits`)
val `scodec-core`     = Def.setting("org.scodec"      %%% "scodec-core"     % V.`scodec-core`)
val `scodec-stream`   = Def.setting("org.scodec"      %%% "scodec-stream"   % V.`scodec-stream`)
val shapeless         = Def.setting("com.chuusai"     %%% "shapeless"       % V.shapeless)
val `log-effect-fs2`  = Def.setting("io.laserdisc"    %%% "log-effect-fs2"  % V.`log-effect-fs2`)
val scalacheck        = Def.setting("org.scalacheck"  %%% "scalacheck"      % V.scalacheck % Test)
val scalatest         = Def.setting("org.scalatest"   %%% "scalatest"       % V.scalatest  % Test)

val `kind-projector-compiler-plugin` = Def.setting {
  compilerPlugin("org.spire-math" % "kind-projector" % V.`kind-projector` cross CrossVersion.binary)
}
val `scalajs-compiler-plugin` = Def.setting {
  compilerPlugin("org.scala-js" % "scalajs-compiler" % scalaJSVersion cross CrossVersion.patch)
}

val coreDeps = Def.Initialize.join {
  Seq(`kind-projector-compiler-plugin`, refined, `scodec-bits`, `scodec-core`, shapeless, scalacheck, scalatest)
}

val fs2Deps = Def.Initialize.join {
  Seq(
    `fs2-core`,
    `fs2-io`,
    `kind-projector-compiler-plugin`,
    kittens,
    `scodec-stream`,
    `log-effect-fs2`,
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

  (coreDeps.value :+ (scalaOrganization.value % "scala-library" % scalaVersion.value))
    .flatMap(JavaDocIo.maybeDocsFor)
    .toMap
}

val versionDependantScalacOptions = Def.setting {
  def versionDependent(scalaVersion: String, flags: Seq[String]) = CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, major)) if major >= 12 =>
      flags ++ Seq(
        "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
        "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
        "-Ywarn-unused:explicits", // Warn if a parameter is unused.
        "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
        "-Ywarn-unused:locals", // Warn if a local definition is unused.
        "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
        "-Ywarn-unused:privates", // Warn if a private member is unused.
        "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
      )
    case _ =>
      flags
        .filterNot(_ == "-Xlint:missing-interpolator") //@implicitNotFound uses ${A} syntax w/o need for s interpolator
  }

  val flags = Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match", // Pattern match may not be typesafe.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Yinduction-heuristics", // Nobody wants recursive implicit searches that last forever, we need TLS for this
    "-Yliteral-types" // Allow inferring singleton types, we need TLS for this in 2.12
  )

  versionDependent(scalaVersion.value, flags)
}

inThisBuild {
  Def.settings(
    organization := "io.laserdisc",
    scalaOrganization := "org.typelevel",
    scalaVersion := "2.12.4-bin-typelevel-4"
  )
}

lazy val commonSettings = Seq(
  crossScalaVersions := Seq("2.11.11-bin-typelevel-4", "2.12.4-bin-typelevel-4"),
  scalacOptions ++= versionDependantScalacOptions.value,
  Compile / console / scalacOptions --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
  Test / console / scalacOptions := (Compile / console / scalacOptions).value
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  Test / publishArtifact := false,
  pomIncludeRepository := (_ => false),
  developers := List(
    Developer("sirocchi", "Julien Sirocchi", "julien.sirocchi@gmail.com", url("https://github.com/sirocchj"))
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
  Test / parallelExecution := false,
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

lazy val allSettings = commonSettings ++ testSettings ++ scaladocSettings ++ publishSettings

lazy val scalaJsTLSSettings = Seq(
  libraryDependencies := `scalajs-compiler-plugin`.value +:
    libraryDependencies.value.filterNot(_.name == `scalajs-compiler-plugin`.value.name)
)

lazy val core = crossProject(JSPlatform, JVMPlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(allSettings)
  .settings(
    name := "laserdisc-core",
    libraryDependencies ++= coreDeps.value,
    Compile / boilerplateSource := baseDirectory.value.getParentFile / "src" / "main" / "boilerplate"
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
  .jsSettings(scalaJsTLSSettings: _*)

lazy val coreJVM = core.jvm.enablePlugins(BoilerplatePlugin)
lazy val coreJS  = core.js.enablePlugins(BoilerplatePlugin)

lazy val fs2 = project
  .in(file("fs2"))
  .dependsOn(coreJVM)
  .settings(allSettings)
  .settings(
    name := "laserdisc-fs2",
    libraryDependencies ++= fs2Deps.value
  )

lazy val `core-bench` = project
  .in(file("benchmarks/core"))
  .dependsOn(coreJVM)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "laserdisc-core-benchmarks",
    publishArtifact := false
  )

lazy val laserdisc = project
  .in(file("."))
  .aggregate(coreJVM, coreJS, fs2)
  .settings(publishSettings)
  .settings(
    publishArtifact := false
  )
