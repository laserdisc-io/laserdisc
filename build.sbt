import sbtcrossproject.CrossProject

val scala_212 = "2.12.20"
val scala_213 = "2.13.15"

val V = new {
  val cats                   = "2.12.0"
  val `cats-effect`          = "3.5.7"
  val `cats-discipline`      = "1.7.0"
  val `discipline-munit`     = "2.0.0"
  val circe                  = "0.14.10"
  val fs2                    = "3.11.0"
  val jedis                  = "3.2.0"
  val kittens                = "3.4.0"
  val `log-effect`           = "0.19.2"
  val logback                = "1.5.15"
  val munit                  = "1.0.3"
  val `munit-cats-effect`    = "2.0.0"
  val `munit-scalacheck`     = "1.0.0"
  val `parallel-collections` = "1.1.0"
  val redis4Cats             = "1.0.0-RC3"
  val refined                = "0.11.3"
  val scalacheck             = "1.18.1"
  val `scala-redis`          = "3.30"
  val `scodec-bits`          = "1.2.1"
  val `scodec-core`          = "1.11.10"
  val `scodec-stream`        = "3.0.2"
  val scredis                = "2.3.3"
  val shapeless              = "2.3.12"
  val slf4j                  = "2.0.16"
}

val D = new {
  val `cats-core`          = Def.setting("org.typelevel" %%% "cats-core" % V.cats)
  val `cats-effect-kernel` = Def.setting("org.typelevel" %%% "cats-effect-kernel" % V.`cats-effect`)
  val `cats-effect-std`    = Def.setting("org.typelevel" %%% "cats-effect-std" % V.`cats-effect`)
  val `cats-effect`        = Def.setting("org.typelevel" %%% "cats-effect" % V.`cats-effect`)
  val `cats-laws`          = Def.setting("org.typelevel" %%% "cats-laws" % V.cats)
  val `circe-core`         = Def.setting("io.circe" %%% "circe-core" % V.circe)
  val `circe-parser`       = Def.setting("io.circe" %%% "circe-parser" % V.circe)
  val `fs2-core`           = Def.setting("co.fs2" %%% "fs2-core" % V.fs2)
  val `fs2-io`             = Def.setting("co.fs2" %%% "fs2-io" % V.fs2)
  val jedis                = Def.setting("redis.clients" % "jedis" % V.jedis)
  val kittens              = Def.setting("org.typelevel" %%% "kittens" % V.kittens)
  val `log-effect-fs2`     = Def.setting("io.laserdisc" %%% "log-effect-fs2" % V.`log-effect`)
  val logback              = Def.setting("ch.qos.logback" % "logback-classic" % V.logback)
  val redis4Cats           = Def.setting("dev.profunktor" %% "redis4cats-effects" % V.redis4Cats)
  val refined              = Def.setting("eu.timepit" %%% "refined" % V.refined)
  val `scala-redis`        = Def.setting("net.debasishg" %% "redisclient" % V.`scala-redis`)
  val `scodec-bits`        = Def.setting("org.scodec" %%% "scodec-bits" % V.`scodec-bits`)
  val `scodec-core`        = Def.setting("org.scodec" %%% "scodec-core" % V.`scodec-core`)
  val `scodec-stream`      = Def.setting("org.scodec" %%% "scodec-stream" % V.`scodec-stream`)
  val scredis              = Def.setting("com.github.scredis" %% "scredis" % V.scredis)
  val shapeless            = Def.setting("com.chuusai" %%% "shapeless" % V.shapeless)
  val `slf4j-api`          = Def.setting("org.slf4j" % "slf4j-api" % V.slf4j)

  val `cats-discipline`    = Def.setting("org.typelevel" %%% "discipline-core" % V.`cats-discipline` % Test)
  val `discipline-munit`   = Def.setting("org.typelevel" %%% "discipline-munit" % V.`discipline-munit` % Test)
  val `circe-generic`      = Def.setting("io.circe" %%% "circe-generic" % V.circe % Test)
  val `refined-scalacheck` = Def.setting("eu.timepit" %%% "refined-scalacheck" % V.refined % Test)
  val scalacheck           = Def.setting("org.scalacheck" %%% "scalacheck" % V.scalacheck % Test)
  val munit                = Def.setting("org.scalameta" %%% "munit" % V.munit % Test)
  val `munit-cats-effect`  = Def.setting("org.typelevel" %%% "munit-cats-effect" % V.`munit-cats-effect` % Test)
  val `munit-scalacheck`   = Def.setting("org.scalameta" %%% "munit-scalacheck" % V.`munit-scalacheck` % Test)

  val core = Def.Initialize.join {
    Seq(
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

  val laws = Def.Initialize.join {
    Seq(
      `cats-core`,
      `cats-laws`,
      `cats-discipline`,
      `discipline-munit`
    )
  }

  val fs2 = Def.Initialize.join {
    Seq(
      `cats-core`,
      `cats-effect`,
      `fs2-core`,
      `fs2-io`,
      kittens,
      `log-effect-fs2`,
      `scodec-stream`,
      scalacheck,
      munit,
      `munit-cats-effect`,
      `munit-scalacheck`
    )
  }

  val `fs2-bench` = Def.Initialize.join {
    Seq(
      jedis,
      redis4Cats,
      `scala-redis`,
      scredis,
      logback,
      `slf4j-api`
    )
  }

  val circe = Def.Initialize.join {
    Seq(
      `circe-core`,
      `circe-parser`,
      `circe-generic`,
      scalacheck,
      munit,
      `munit-scalacheck`
    )
  }
}

// found this suggestion here
// https://github.com/scala/bug/issues/10682#issuecomment-1005114838
// and seems from here that it resolves
// https://github.com/scala/bug/issues/10682#issuecomment-1005143679
// It's a mitigation for errors like
// https://github.com/laserdisc-io/laserdisc/actions/runs/3586636347/jobs/6036173999#step:6:396
// tracked here:
// https://github.com/fthomas/refined/issues/260
// https://github.com/scala/bug/issues/9218
// and for other sporadic compiler crashes
Global / concurrentRestrictions += Tags.limit(Tags.Compile, 1)
Global / excludeLintKeys += scalaJSLinkerConfig

ThisBuild / tlBaseVersion              := "0.7"
ThisBuild / tlCiReleaseBranches        := Seq("master")
ThisBuild / sonatypeCredentialHost     := Sonatype.sonatypeLegacy
ThisBuild / organization               := "io.laserdisc"
ThisBuild / organizationName           := "LaserDisc"
ThisBuild / licenses                   := Seq(License.MIT)
ThisBuild / startYear                  := Some(2018)
ThisBuild / developers                 := List(tlGitHubDev("sirocchj", "Julien Sirocchi"), tlGitHubDev("barambani", "Filippo Mariotti"))
ThisBuild / crossScalaVersions         := Seq(scala_212, scala_213)
ThisBuild / scalaVersion               := scala_213
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"), JavaSpec.temurin("17"), JavaSpec.temurin("21"))

ThisBuild / githubWorkflowBuildPreamble ++= Seq(
  WorkflowStep.Run(
    commands = List("docker run -p 6379:6379 -d redis:alpine"),
    name = Some("Start up Redis")
  ),
  WorkflowStep.Run(
    commands = List("docker run -p 6380:6379 -d eqalpha/keydb"),
    name = Some("Start up KeyDb")
  ),
  WorkflowStep.Run(
    commands = List("docker run -p 6381:6379 -e ALLOW_EMPTY_PASSWORD=yes -d bitnami/valkey:latest"),
    name = Some("Start up Valkey")
  )
)
ThisBuild / mergifyLabelPaths    := Map.empty
ThisBuild / mergifyStewardConfig := Some(MergifyStewardConfig(action = MergifyAction.Merge(Some("squash"))))

lazy val commonSettings = Seq(
  coverageEnabled          := false,
  coverageFailOnMinimum    := false,
  coverageHighlighting     := true,
  coverageMinimumStmtTotal := 60,
  headerEndYear            := Some(2025),
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
)

def laserdiscCrossModule(path: String) = {
  val pathBits    = path.split("[-/]")
  val id          = pathBits.reduce(_ + _.capitalize)
  val namePartial = pathBits.reverse.mkString("-")
  CrossProject(id, file(path))(JVMPlatform, JSPlatform)
    .crossType(CrossType.Pure)
    .withoutSuffixFor(JVMPlatform)
    .settings(commonSettings)
    .settings(
      name := s"laserdisc-$namePartial",
      scalacOptions ++= {
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, major)) if major >= 13 => Seq("-Wconf:cat=unused-nowarn:s")
          case _                               => Seq.empty
        }
      }
    )
    .jvmSettings(
      coverageEnabled := true,
      Test / fork     := true
    )
}

lazy val laserdisc = tlCrossRootProject
  .aggregate(core, laws, fs2, cli, circe)
  .settings(commonSettings)

lazy val core = laserdiscCrossModule("core")
  .enablePlugins(BoilerplatePlugin)
  .settings(
    libraryDependencies ++= D.core.value,
    Compile / boilerplateSource := crossProjectBaseDirectory.value / "src" / "main" / "boilerplate",
    Test / boilerplateSource    := crossProjectBaseDirectory.value / "src" / "test" / "boilerplate"
  )
  .jvmSettings(
    javaOptions += "-Djava.net.preferIPv4Stack=true",
    initialCommands := s"""
      |import laserdisc._
      |import laserdisc.auto._
      |import laserdisc.all._
      |import shapeless._
      |""".stripMargin
  )

lazy val laws = laserdiscCrossModule("laws")
  .dependsOn(core)
  .enablePlugins(NoPublishPlugin)
  .settings(
    libraryDependencies ++= D.core.value ++ D.laws.value
  )

lazy val fs2 = laserdiscCrossModule("fs2")
  .dependsOn(core)
  .settings(
    libraryDependencies ++= D.fs2.value
  )
  .jvmSettings(
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, major)) if major >= 13 =>
          Seq("org.scala-lang.modules" %%% "scala-parallel-collections" % V.`parallel-collections` % Test)
        case _ =>
          Seq.empty
      }
    }
  )

lazy val cli = laserdiscCrossModule("cli")
  .dependsOn(fs2)
  .settings(
    libraryDependencies ++= D.fs2.value
  )

lazy val circe = laserdiscCrossModule("circe")
  .dependsOn(core)
  .settings(
    libraryDependencies ++= D.circe.value
  )

lazy val `core-bench` = project
  .in(file("benchmarks/core"))
  .dependsOn(core.jvm % "compile->test;compile->compile")
  .enablePlugins(JmhPlugin, NoPublishPlugin)

lazy val `fs2-bench` = project
  .in(file("benchmarks/fs2"))
  .dependsOn(fs2.jvm)
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(
    libraryDependencies ++= D.`fs2-bench`.value
  )
