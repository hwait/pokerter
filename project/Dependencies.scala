import sbt._

object Dependencies {
  val zioVersion= "2.0.0-RC6"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.11"
  lazy val zlibs = Seq(
    "dev.zio" %% "zio" % zioVersion, 
    "dev.zio" %% "zio-streams" % zioVersion, 
    "dev.zio" %% "zio-test" % zioVersion 
  )
}
