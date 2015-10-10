import sbt._
import sbt.Keys._

import com.typesafe.sbt.pgp.PgpKeys
import com.typesafe.sbt.SbtGit.GitKeys.gitRemoteRepo
import com.typesafe.sbt.SbtGhPages
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtSite
import pl.project13.scala.sbt.JmhPlugin
import sbtrelease.ReleasePlugin.autoImport._
import spray.boilerplate.BoilerplatePlugin._
import xerial.sbt.Sonatype._

object SqlestBuild extends Build {

  lazy val testDependencies = Seq(
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "com.chuusai" %% "shapeless" % "2.2.5" % "test"
  )

  lazy val root = Project(
    id = "root",
    base = file("."),
    aggregate = Seq(core, queries, extractors, examples),
    settings = commonSettings ++ Seq(
      moduleName := "root",

      publish := (),
      publishLocal := ()
    )
  )

  lazy val bench = Project(
    id = "bench",
    base = file("bench"),

    settings = commonSettings ++ Seq(
      moduleName := "bench",

      libraryDependencies ++= Seq(
        "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
      )
    )
  ).dependsOn(extractors).enablePlugins(JmhPlugin)

  lazy val core = Project(
    id = "core",
    base = file("core"),

    settings = commonSettings ++ scaladocSettings ++ Seq(
      moduleName := "core",

      libraryDependencies ++= testDependencies ++ Seq(
        "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
      )
    )
  ).dependsOn(queries, extractors)

  lazy val queries = Project(
    id = "queries",
    base = file("queries"),

    settings = commonSettings ++ scaladocSettings ++ Boilerplate.settings ++ Seq(
      moduleName := "queries",

      libraryDependencies ++= testDependencies ++ Seq(
        "joda-time" % "joda-time" % "2.3",
        "org.joda" % "joda-convert" % "1.6",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
      )
    )
  ).dependsOn(extractors)

  lazy val extractors = Project(
    id = "extractors",
    base = file("extractors"),

    settings = commonSettings ++ scaladocSettings ++ Boilerplate.settings ++ Seq(
      moduleName := "sqlest-extractors",

      libraryDependencies ++= testDependencies ++ Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value,
        "joda-time" % "joda-time" % "2.3",
        "org.joda" % "joda-convert" % "1.6",
        "org.spire-math" %% "cats" % "0.2.0",
        compilerPlugin("org.spire-math" %% "kind-projector" % "0.6.3")
      )
    )
  )

  lazy val examples = Project(
    id = "examples",
    base = file("examples"),

    settings = commonSettings ++ Seq(
      libraryDependencies += "com.h2database" % "h2" % "1.4.180",

      publish := (),
      publishLocal := ()
    )
  ).dependsOn(core)

  def commonSettings = SbtScalariform.scalariformSettings ++ publishingSettings ++ Seq(
    organization := "uk.co.jhc",
    scalaVersion := "2.11.7",
    scalacOptions ++= Seq(
      "-unchecked", 
      "-deprecation", 
      "-feature", 
      "-Xfatal-warnings", 
      "-language:implicitConversions", 
      "-language:existentials"
    ),
    initialCommands in (Test, console) := 
      """ammonite.repl.Repl.run("repl.frontEnd() = ammonite.repl.frontend.FrontEnd.JLineWindows")"""
  )

  def scaladocSettings = SbtSite.site.settings ++ SbtSite.site.includeScaladoc() ++ SbtGhPages.ghpages.settings ++ Seq(
    gitRemoteRepo := "git@github.com:jhc-systems/sqlest.git"
  )

  def publishingSettings = sonatypeSettings ++ Seq(
    // Publishing - http://www.scala-sbt.org/0.13/docs/Using-Sonatype.html
    releasePublishArtifactsAction := PgpKeys.publishSigned.value,
    publishMavenStyle := true,
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    credentials := Seq(Seq("SONATYPE_USER", "SONATYPE_PASSWORD").map(key => sys.env.get(key)) match {
      case Seq(Some(user), Some(password)) =>
        Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", user, password)
      case _                           =>
        Credentials(Path.userHome / ".ivy2" / ".credentials")
    }),
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <url>https://github.com/jhc-systems/sqlest</url>
      <licenses>
        <license>
          <name>Apache License, Version 2.0</name>
          <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com/jhc-systems/sqlest.git</url>
        <connection>scm:git:git@github.com/jhc-systems/sqlest.git</connection>
      </scm>
      <developers>
        <developer>
          <id>davegurnell</id>
          <name>Dave Gurnell</name>
        </developer>
        <developer>
          <id>brendanator</id>
          <name>Brendan Maginnis</name>
        </developer>
      </developers>)
  )
}
