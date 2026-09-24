val giteaMaven = "https://gitea.local.vgerasimov.dev/api/packages/wlad031/maven"
val artifactVersion = sys.env.getOrElse("VERSION", "0.1.4-SNAPSHOT")
val giteaCredentials = for {
  username <- sys.env.get("GITEA_USERNAME")
  token <- sys.env.get("GITEA_TOKEN")
} yield Credentials("Gitea Package API", "gitea.local.vgerasimov.dev", username, token)

val root = project
  .enablePlugins(BuildInfoPlugin)
  .in(file("."))
  .settings(
    scalaVersion := "3.8.3",
    organization := "dev.vgerasimov",
    name := "lizp",
    version := artifactVersion,
    resolvers += "gitea" at giteaMaven,
    publishTo := Some("gitea" at giteaMaven),
    publishMavenStyle := true,
    credentials ++= giteaCredentials,
    scalacOptions ++= Seq(
      "-rewrite",
      "-source", "future",
      "-Werror",
    ),
    libraryDependencies ++= {
      val munitVersion = "0.7.29"
      Seq(
        "org.scalameta"    %% "munit"            % munitVersion % Test,
        "org.scalameta"    %% "munit-scalacheck" % munitVersion % Test,
        "dev.vgerasimov"   %% "slowparse"        % "0.2.1",
        "com.github.scopt" %% "scopt"            % "4.0.1"
      )
    },
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "dev.vgerasimov.lizp.build",
    assembly / mainClass := Some("dev.vgerasimov.lizp.run"),
  )
  
