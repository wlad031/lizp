val root = project
  .enablePlugins(BuildInfoPlugin)
  .in(file("."))
  .settings(
    scalaVersion := "3.1.0",
    organization := "dev.vgerasimov",
    name := "lizp",
    version := "0.1.0",
    githubOwner := "wlad031",
    githubRepository := "lizp",
    resolvers += Resolver.githubPackages("wlad031"),
    scalacOptions ++= Seq(
      "-rewrite",
      "-source", "future",
      "-Xfatal-warnings",
    ),
    libraryDependencies ++= {
      val munitVersion = "0.7.29"
      Seq(
        "org.scalameta"    %% "munit"            % munitVersion % Test,
        "org.scalameta"    %% "munit-scalacheck" % munitVersion % Test,
        "dev.vgerasimov"   %% "slowparse"        % "0.1.2",
        "com.github.scopt" %% "scopt"            % "4.0.1"
      )
    },
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "dev.vgerasimov.lizp.build",
    assembly / mainClass := Some("dev.vgerasimov.lizp.run"),
  )
  
