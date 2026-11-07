import laika.helium.config.{HeliumIcon, IconLink}
import sbtcrossproject.CrossProject

val scala_212 = "2.12.21"
val scala_213 = "2.13.18"
val scala_3   = "3.9.0"

val V = new {
  val cats                   = "2.13.0"
  val `cats-effect`          = "3.7.1"
  val `cats-discipline`      = "1.7.0"
  val `collection-compat`    = "2.14.0"
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
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"), JavaSpec.temurin("21"))

ThisBuild / coverageFailOnMinimum      := true
ThisBuild / coverageHighlighting       := true
ThisBuild / coverageMinimumStmtTotal   := 90
ThisBuild / coverageMinimumBranchTotal := 80

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
  WorkflowStep.Run(commands = List("docker run -p 6379:6379 -d redis:alpine"), name = Some("Start up Redis")),
  WorkflowStep.Run(commands = List("docker run -p 6380:6379 -d eqalpha/keydb"), name = Some("Start up KeyDb")),
  WorkflowStep.Run(commands = List("docker run -p 6381:6379 -d valkey/valkey:alpine"), name = Some("Start up Valkey")),
  WorkflowStep.Run(
    commands = List("docker run -p 6382:6379 --ulimit memlock=-1 -d docker.dragonflydb.io/dragonflydb/dragonfly:latest"),
    name = Some("Start up Dragonfly")
  ),
  WorkflowStep.Run(
    commands =
      List("docker run -p 6383:6379 --ulimit memlock=-1 -d docker.dragonflydb.io/dragonflydb/dragonfly:latest --cluster_mode=emulated"),
    name = Some("Start up Dragonfly in cluster mode")
  ),
  startUpCluster("Redis", "redis:alpine", "redis-server", 7100),
  startUpCluster("KeyDb", "eqalpha/keydb", "keydb-server", 7110),
  startUpCluster("Valkey", "valkey/valkey:alpine", "valkey-server", 7120)
)

ThisBuild / githubWorkflowAddedJobs ++= List(
  WorkflowJob(
    id = "integration",
    name = "Integration tests",
    scalas = githubWorkflowScalaVersions.value.toList,
    javas = githubWorkflowJavaVersions.value.toList,
    matrixAdds = Map("project" -> List("integration", "integrationJS", "integrationNative")),
    matrixExcs = List(
      MatrixExclude(Map("scala" -> "2.12", "java" -> githubWorkflowJavaVersions.value.last.render)),
      MatrixExclude(Map("scala" -> "3", "java" -> githubWorkflowJavaVersions.value.last.render)),
      MatrixExclude(Map("project" -> "integrationJS", "java" -> githubWorkflowJavaVersions.value.last.render)),
      MatrixExclude(Map("project" -> "integrationNative", "java" -> githubWorkflowJavaVersions.value.last.render))
    ),
    steps = githubWorkflowJobSetup.value.toList ++ startUpRedisServers :+
      WorkflowStep.Sbt(
        List("project ${{ matrix.project }}", "headerCheckAll", "scalafmtCheckAll", "test"),
        name = Some("Check headers and formatting, and run the tests")
      )
  ),
  WorkflowJob(
    id = "coverage",
    name = "Check coverage",
    scalas = List(scala_213),
    javas = List(githubWorkflowJavaVersions.value.head),
    steps = githubWorkflowJobSetup.value.toList ++ startUpRedisServers :+
      WorkflowStep.Sbt(
        List(
          "coverage",
          "core/test",
          "laws/test",
          "fs2/test",
          "circe/test",
          "cli/test",
          "integration/test",
          "laserdiscJVM/coverageAggregate"
        ),
        name = Some("Run tests with coverage")
      )
  )
)
ThisBuild / tlSitePublishBranch := Some("master")

ThisBuild / mergifyLabelPaths    := Map.empty
ThisBuild / mergifyStewardConfig := Some(MergifyStewardConfig(action = MergifyAction.Merge(Some("squash"))))

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

lazy val cli = laserdiscCrossModule("cli")
  .dependsOn(fs2)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2"        %%% "fs2-io"      % V.fs2,
      "org.typelevel" %%% "cats-core"   % V.cats,
      "org.typelevel" %%% "cats-effect" % V.`cats-effect`,
      "org.scalameta" %%% "munit"       % V.munit % Test
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true
  )

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
