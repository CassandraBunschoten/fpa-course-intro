import sbt._

object Dependencies {
  lazy val scalaTest     = "org.scalatest"  %% "scalatest"      % "3.0.5"
  lazy val scalaCheck    = "org.scalacheck" %% "scalacheck"     % "1.14.0"
  lazy val catsCore      = "org.typelevel"  %% "cats-core"      % "1.5.0"
  lazy val catsEffect    = "org.typelevel"  %% "cats-effect"    % "1.1.0"
  lazy val fastparse     = "com.lihaoyi"    %% "fastparse"      % "2.0.5"
  lazy val uPickle       = "com.lihaoyi"    %% "upickle"        % "0.7.1"
  lazy val kindProjector = "org.spire-math" %% "kind-projector" % "0.9.8"
}
