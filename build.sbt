import laika.helium.config.{HeliumIcon, IconLink}
import org.typelevel.sbt.gha.{PermissionValue, Permissions}
import sbtcrossproject.CrossProject

val scala_212 = "2.12.21"
val scala_213 = "2.13.18"
val scala_3   = "3.9.0"

val V = new {
  val cats                   = "2.13.0"
  val `cats-effect`          = "3.7.1"
  val `cats-discipline`      = "1.7.0"
  val `collection-compat`    = "2.14.0"
  val decline                = "2.6.2"
  val `discipline-munit`     = "2.0.0"
  val circe                  = "0.14.17"
  val fs2                    = "3.14.0"
  val jedis                  = "3.2.0"
  val `log-effect`           = "0.20.0"
  val logback                = "1.6.6"
  val munit                  = "1.3.6"
  val `munit-cats-effect`    = "2.2.1"
  val `munit-scalacheck`     = "1.3.1"
  val `parallel-collections` = "1.3.0"
  val redis4Cats             = "2.0.3"
  val refined                = "0.11.4"
  val scalacheck             = "1.20.0"
  val `scala-redis`          = "3.42"
  val `scodec-bits`          = "1.2.5"
  val `scodec-core_scala2`   = "1.11.11"
  val `scodec-core_scala3`   = "2.3.3"
  val scredis                = "2.4.3"
  val shapeless              = "2.3.13"
  val iron                   = "3.4.1"
  val slf4j                  = "2.0.20"
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

ThisBuild / tlBaseVersion              := "0.8"
ThisBuild / tlCiReleaseBranches        := Seq("master")
ThisBuild / organization               := "io.laserdisc"
ThisBuild / organizationName           := "LaserDisc"
ThisBuild / licenses                   := Seq(License.MIT)
ThisBuild / startYear                  := Some(2018)
ThisBuild / developers                 := List(tlGitHubDev("sirocchj", "Julien Sirocchi"), tlGitHubDev("barambani", "Filippo Mariotti"))
ThisBuild / crossScalaVersions         := Seq(scala_212, scala_213, scala_3)
ThisBuild / scalaVersion               := scala_213
ThisBuild / tlJdkRelease               := Some(17)
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"), JavaSpec.temurin("21"), JavaSpec.temurin("25"))

ThisBuild / coverageFailOnMinimum      := true
ThisBuild / coverageHighlighting       := true
ThisBuild / coverageMinimumStmtTotal   := 90
ThisBuild / coverageMinimumBranchTotal := 80

// the servers the integration tests run against, at fixed versions, so that a new release doesn't change what CI finds without a
// commit saying so. KeyDB has no tag for its version that covers both amd64 and arm64, so it's fixed by the digest of 6.3.4
val redisImage     = "redis:8.10.3-alpine"
val keydbImage     = "eqalpha/keydb@sha256:6537505c42355ca1f571276bddf83f5b750f760f07b2a185a676481791e388ac"
val valkeyImage    = "valkey/valkey:9.1.3-alpine"
val dragonflyImage = "docker.dragonflydb.io/dragonflydb/dragonfly:v2.0.2"

// four nodes for ClusterClientSpec to make a cluster of: they're in one container so that they can reach each other on 127.0.0.1.
// Not from port 7000, which AirPlay Receiver has on macOS
def startUpCluster(name: String, image: String, server: String, firstPort: Int) = {
  val ports           = (firstPort until firstPort + 4).toList
  def node(port: Int) = s"$server --port $port --cluster-enabled yes --cluster-config-file nodes-$port.conf --protected-mode no"
  val published       = ports.map(p => s"-p $p:$p").mkString(" ")
  val nodes           = (ports.init.map(node(_) + " --daemonize yes") :+ s"exec ${node(ports.last)}").mkString("; ")
  WorkflowStep.Run(
    commands = List(s"docker run $published -d --entrypoint sh $image -c '$nodes'"),
    name = Some(s"Start up a $name cluster")
  )
}

val startUpRedisServers = List(
  WorkflowStep.Run(commands = List(s"docker run -p 6379:6379 -d $redisImage"), name = Some("Start up Redis")),
  WorkflowStep.Run(commands = List(s"docker run -p 6380:6379 -d $keydbImage"), name = Some("Start up KeyDb")),
  WorkflowStep.Run(commands = List(s"docker run -p 6381:6379 -d $valkeyImage"), name = Some("Start up Valkey")),
  WorkflowStep.Run(
    commands = List(s"docker run -p 6382:6379 --ulimit memlock=-1 -d $dragonflyImage"),
    name = Some("Start up Dragonfly")
  ),
  WorkflowStep.Run(
    commands = List(s"docker run -p 6383:6379 --ulimit memlock=-1 -d $dragonflyImage --cluster_mode=emulated"),
    name = Some("Start up Dragonfly in cluster mode")
  ),
  // only TLS, for TlsSpec, with the certificate in test-setup/tls, which is for localhost and not for 127.0.0.1
  WorkflowStep.Run(
    commands = List(
      "docker run -p 6390:6379 -v \"$PWD/test-setup/tls:/tls:ro\" -d " + redisImage +
        " --port 0 --tls-port 6379 --tls-cert-file /tls/server.crt --tls-key-file /tls/server.key --tls-ca-cert-file /tls/ca.crt" +
        " --tls-auth-clients no"
    ),
    name = Some("Start up Redis with TLS")
  ),
  startUpCluster("Redis", redisImage, "redis-server", 7100),
  startUpCluster("KeyDb", keydbImage, "keydb-server", 7110),
  startUpCluster("Valkey", valkeyImage, "valkey-server", 7120)
)

// the jobs only read the code, except publishing the site and submitting the dependencies, which write to the repository
val readOnly = Permissions.Specify.defaultRestrictive.withContents(PermissionValue.Read)
ThisBuild / githubWorkflowPermissions := Some(readOnly)

// artifacts only carry the build from one job to a later one in the same run, so a week leaves time to re-run a failed job
def expiring(step: WorkflowStep): WorkflowStep = step match {
  case use: WorkflowStep.Use if uploadsArtifact(use.ref) => use.updatedParams("retention-days", "7")
  case other                                             => other
}
def uploadsArtifact(ref: UseRef): Boolean = ref match {
  case UseRef.Public("actions", "upload-artifact", _) => true
  case _                                              => false
}

ThisBuild / githubWorkflowGeneratedCI ~= (_.map { job =>
  if (Set("site", "dependency-submission")(job.id)) job.withPermissions(Some(readOnly.withContents(PermissionValue.Write))) else job
}.map(job => job.withSteps(job.steps.map(expiring))))

// the Clean workflow deletes every artifact in the repository on each push, those of runs still going included, and can't be given
// permissions of its own. Uploads expire instead
ThisBuild / githubWorkflowIncludeClean := false

// the Scala Native binaries that use TLS link s2n-tls, which CI installs with Homebrew for the jobs that build them
ThisBuild / nativeBrewInstallCond := Some("matrix.project == 'laserdiscNative' || matrix.project == 'integrationNative'")
ThisBuild / githubWorkflowBuildPreamble ++= nativeBrewInstallWorkflowSteps.value

ThisBuild / githubWorkflowAddedJobs ++= List(
  WorkflowJob(
    id = "integration",
    name = "Integration tests",
    scalas = githubWorkflowScalaVersions.value.toList,
    javas = githubWorkflowJavaVersions.value.toList,
    matrixAdds = Map("project" -> List("integration", "integrationJS", "integrationNative")),
    // the later Java versions only run Scala 2.13 on the JVM, as the build job does
    matrixExcs = githubWorkflowJavaVersions.value.toList.tail.flatMap { java =>
      List(
        MatrixExclude(Map("scala" -> "2.12", "java" -> java.render)),
        MatrixExclude(Map("scala" -> "3", "java" -> java.render)),
        MatrixExclude(Map("project" -> "integrationJS", "java" -> java.render)),
        MatrixExclude(Map("project" -> "integrationNative", "java" -> java.render))
      )
    },
    steps = githubWorkflowJobSetup.value.toList ++ nativeBrewInstallWorkflowSteps.value ++ startUpRedisServers :+
      WorkflowStep.Sbt(
        List("project ${{ matrix.project }}", "headerCheckAll", "scalafmtCheckAll", "test"),
        name = Some("Check headers and formatting, and run the tests")
      )
  ),
  // each version's import against a server of that version: see test-setup/matrix.sh
  WorkflowJob(
    id = "server-versions",
    name = "Server versions",
    scalas = List(scala_3),
    javas = List(githubWorkflowJavaVersions.value.head),
    matrixAdds = Map(
      "import" -> (List("6_2", "7_0", "7_2", "7_4", "8_0", "8_2", "8_4", "8_6", "8_8", "8_10").map("redis_" + _) ++
        List("7_2", "8_0", "8_1", "9_0", "9_1").map("valkey_" + _))
    ),
    matrixFailFast = Some(false),
    steps = githubWorkflowJobSetup.value.toList :+
      WorkflowStep.Run(commands = List("test-setup/matrix.sh ${{ matrix.import }}"), name = Some("Test the import against its server"))
  ),
  WorkflowJob(
    id = "coverage",
    name = "Check coverage",
    scalas = List(scala_213),
    javas = List(githubWorkflowJavaVersions.value.head),
    steps = githubWorkflowJobSetup.value.toList ++ startUpRedisServers :+
      WorkflowStep.Sbt(
        // each published module has to reach the minimums with its own tests: tests also count towards what they depend on, so dependencies go first
        List(
          "coverage",
          "core/test",
          "core/coverageReport",
          "circe/test",
          "circe/coverageReport",
          "fs2/test",
          "fs2/coverageReport",
          "cli/test",
          "cli/coverageReport",
          "laws/test",
          "integration/test",
          "laserdiscJVM/coverageAggregate"
        ),
        name = Some("Run tests with coverage")
      )
  ),
  // the one check for master's protection and for Mergify to require, so adding a version to a matrix doesn't need them changed. It
  // runs no sbt, so its matrix is fixed: its check's name, CI passed (ubuntu-22.04, 2.13, temurin@17), stays the same
  WorkflowJob(
    id = "ci-passed",
    name = "CI passed",
    cond = Some("always()"),
    needs = List("build", "integration", "server-versions", "coverage", "site"),
    scalas = List("2.13"),
    javas = List(JavaSpec.temurin("17")),
    env = Map("RESULTS" -> "${{ join(needs.*.result, ' ') }}"),
    steps = List(
      WorkflowStep.Run(
        commands = List("""for result in $RESULTS; do [ "$result" = success ] || { echo "a job ended with $result"; exit 1; }; done"""),
        name = Some("Check that every job succeeded")
      )
    )
  )
)

// publishing waits for every check, through the job that gathers them, not only for the build
ThisBuild / githubWorkflowPublishNeeds := Seq("ci-passed")

ThisBuild / tlSitePublishBranch := Some("master")

ThisBuild / mergifyLabelPaths    := Map.empty
ThisBuild / mergifyStewardConfig := Some(MergifyStewardConfig(action = MergifyAction.Merge(Some("squash"))))
ThisBuild / mergifyRequiredJobs  := Seq("ci-passed")

lazy val commonSettings = Seq(
  headerEndYear := Some(2026)
)

def laserdiscCrossModule(path: String, crossType: CrossType = CrossType.Pure) = {
  val pathBits    = path.split("[-/]")
  val id          = pathBits.reduce(_ + _.capitalize)
  val namePartial = pathBits.reverse.mkString("-")
  CrossProject(id, file(path))(JVMPlatform, JSPlatform, NativePlatform)
    .crossType(crossType)
    .withoutSuffixFor(JVMPlatform)
    .settings(commonSettings)
    .settings(
      name := s"laserdisc-$namePartial",
      // sbt-typelevel enables kind-projector with * placeholders: switch it to _ placeholders
      scalacOptions ++= (if (tlIsScala3.value) Seq("-Xkind-projector:underscores") else Seq("-P:kind-projector:underscore-placeholders")),
      scalacOptions --= (if (tlIsScala3.value) Seq("-Xkind-projector") else Seq.empty),
      // sbt-typelevel's console options leave out compiler plugins, and Scala 2.12 stops on an option for a missing plugin
      Compile / console / scalacOptions -= "-P:kind-projector:underscore-placeholders",
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.13") Seq("-Wconf:cat=unused-nowarn:s") else Seq.empty)
    )
    .jvmSettings(
      Test / fork := true
    )
    .jsSettings(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    )
}

lazy val laserdisc = tlCrossRootProject
  .aggregate(core, laws, fs2, cli, circe, `core-bench`, `fs2-bench`)
  .settings(commonSettings)

lazy val core = laserdiscCrossModule("core", CrossType.Full)
  .settings(
    libraryDependencies ++= Seq(
      "org.scodec"     %%% "scodec-bits"      % V.`scodec-bits`,
      "org.scalacheck" %%% "scalacheck"       % V.scalacheck         % Test,
      "org.scalameta"  %%% "munit"            % V.munit              % Test,
      "org.scalameta"  %%% "munit-scalacheck" % V.`munit-scalacheck` % Test
    ),
    libraryDependencies ++= {
      if (tlIsScala3.value)
        Seq("org.scodec" %%% "scodec-core" % V.`scodec-core_scala3`, "io.github.iltotore" %%% "iron" % V.iron)
      else
        Seq(
          "org.scodec"   %%% "scodec-core"        % V.`scodec-core_scala2`,
          "com.chuusai"  %%% "shapeless"          % V.shapeless,
          "eu.timepit"   %%% "refined"            % V.refined,
          "org.scala-lang" % "scala-reflect"      % scalaVersion.value,
          "eu.timepit"   %%% "refined-scalacheck" % V.refined % Test
        )
    }
  )
  .jvmSettings(
    javaOptions += "-Djava.net.preferIPv4Stack=true",
    initialCommands := s"""
      |import laserdisc.*
      |import laserdisc.auto.*
      |import laserdisc.redis_8_10.*
      |""".stripMargin
  )

lazy val laws = laserdiscCrossModule("laws")
  .dependsOn(core)
  .enablePlugins(NoPublishPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel"  %%% "cats-core"        % V.cats,
      "org.typelevel"  %%% "cats-laws"        % V.cats,
      "org.scalacheck" %%% "scalacheck"       % V.scalacheck         % Test,
      "org.scalameta"  %%% "munit"            % V.munit              % Test,
      "org.scalameta"  %%% "munit-scalacheck" % V.`munit-scalacheck` % Test,
      "org.typelevel"  %%% "discipline-core"  % V.`cats-discipline`  % Test,
      "org.typelevel"  %%% "discipline-munit" % V.`discipline-munit` % Test
    )
  )

lazy val fs2 = laserdiscCrossModule("fs2", CrossType.Full)
  .dependsOn(core)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2"        %%% "fs2-core"          % V.fs2,
      "co.fs2"        %%% "fs2-io"            % V.fs2,
      "co.fs2"        %%% "fs2-scodec"        % V.fs2,
      "io.laserdisc"  %%% "log-effect-fs2"    % V.`log-effect`,
      "org.typelevel" %%% "cats-core"         % V.cats,
      "org.typelevel" %%% "cats-effect"       % V.`cats-effect`,
      "org.scalameta" %%% "munit"             % V.munit               % Test,
      "org.typelevel" %%% "munit-cats-effect" % V.`munit-cats-effect` % Test
    )
  )

// tests against real servers: not in the root's aggregate, so that `test` needs no containers. See docs/contributing.md
lazy val integration = laserdiscCrossModule("integration", CrossType.Full)
  .dependsOn(fs2)
  .enablePlugins(NoPublishPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"             % V.munit               % Test,
      "org.typelevel" %%% "munit-cats-effect" % V.`munit-cats-effect` % Test
    )
  )
  .jvmSettings(
    libraryDependencies ++= {
      if (scalaBinaryVersion.value == "2.12") Seq.empty
      else Seq("org.scala-lang.modules" %%% "scala-parallel-collections" % V.`parallel-collections` % Test)
    }
  )
  // TlsSpec connects with TLS, which needs s2n-tls on Scala Native
  .nativeEnablePlugins(ScalaNativeBrewedConfigPlugin)
  .nativeSettings(nativeBrewFormulas += "s2n")

lazy val cli = laserdiscCrossModule("cli", CrossType.Full)
  .dependsOn(fs2 % "compile->compile;test->test")
  .settings(
    libraryDependencies ++= Seq(
      "com.monovore"  %%% "decline"           % V.decline,
      "co.fs2"        %%% "fs2-io"            % V.fs2,
      "org.typelevel" %%% "cats-core"         % V.cats,
      "org.typelevel" %%% "cats-effect"       % V.`cats-effect`,
      "org.scalameta" %%% "munit"             % V.munit               % Test,
      "org.typelevel" %%% "munit-cats-effect" % V.`munit-cats-effect` % Test
    )
  )
  // run in a process of its own, reading sbt's input, so that what's typed reaches it and Ctrl-D ends it
  .jvmSettings(
    Compile / run / fork         := true,
    Compile / run / connectInput := true
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )
  // --tls needs s2n-tls on Scala Native, so the CLI's binary always links it
  .nativeEnablePlugins(ScalaNativeBrewedConfigPlugin)
  .nativeSettings(nativeBrewFormulas += "s2n")

lazy val circe = laserdiscCrossModule("circe", CrossType.Full)
  .dependsOn(core)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe"       %%% "circe-core"       % V.circe,
      "io.circe"       %%% "circe-parser"     % V.circe,
      "io.circe"       %%% "circe-generic"    % V.circe              % Test,
      "org.scalacheck" %%% "scalacheck"       % V.scalacheck         % Test,
      "org.scalameta"  %%% "munit"            % V.munit              % Test,
      "org.scalameta"  %%% "munit-scalacheck" % V.`munit-scalacheck` % Test
    )
  )

lazy val `core-bench` = project
  .in(file("benchmarks/core"))
  .dependsOn(core.jvm % "compile->test;compile->compile")
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(commonSettings)

lazy val `fs2-bench` = project
  .in(file("benchmarks/fs2"))
  .dependsOn(fs2.jvm)
  .enablePlugins(JmhPlugin, NoPublishPlugin)
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "ch.qos.logback"       % "logback-classic" % V.logback,
      ("com.github.scredis" %% "scredis"         % V.scredis)
        .cross(CrossVersion.for3Use2_13)
        .excludeAll("org.scala-lang.modules" % "scala-collection-compat_2.13"),
      "dev.profunktor"         %% "redis4cats-effects"      % V.redis4Cats,
      ("net.debasishg"         %% "redisclient"             % V.`scala-redis`).cross(CrossVersion.for3Use2_13),
      "org.slf4j"               % "slf4j-api"               % V.slf4j,
      "redis.clients"           % "jedis"                   % V.jedis,
      "org.scala-lang.modules" %% "scala-collection-compat" % V.`collection-compat`
    )
  )

lazy val docs = project
  .in(file("site"))
  .enablePlugins(TypelevelSitePlugin)
  .dependsOn(core.jvm, fs2.jvm, circe.jvm)
  .settings(
    // mdoc on Scala 2 trips over refined's class files (see the comment on concurrentRestrictions), Scala 3 doesn't use refined
    tlSiteGenerate := List(WorkflowStep.Sbt(List(s"++$scala_3", "docs/tlSite"), name = Some("Generate site"))),
    tlSiteHelium   := tlSiteHelium.value.site.topNavigationBar(
      homeLink = IconLink.external("https://github.com/laserdisc-io/laserdisc", HeliumIcon.home)
    )
  )
