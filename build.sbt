//Christoph Knabe, 2018-05-18

//Zwischen einzelnen Schlüsseldefinitionen mit := immer eine Leerzeile lassen!

name := "for-comprehension"

version := "0.2"

scalaVersion := "2.12.4"

scalacOptions ++= Seq("-encoding", "UTF-8") //for file portability

//ScalaTest integrated according to http://www.scalatest.org/user_guide/using_scalatest_with_sbt
//but without Scalactic.

libraryDependencies ++= Seq(
  //vendor %% artifact % version % scope
  "org.scalatest" %% "scalatest" % "3.0.4" % Compile
  /*The operator %% builds an artifact name from the specified scalaVersionDependentArtifact name, 
  * an underscore sign, and the upper mentioned scalaVersion. 
  * So the artifact name will result here in scalatest_2.12,
  * as the last number in a Scala version is not API relevant.
  */
)

//See http://www.scalatest.org/user_guide/using_scalatest_with_sbt
logBuffered in Test := false

//Tell the SBT Eclipse plugin to download all sources along with binary .jar files and make them available for source code navigation. For this the SBT Eclipse plugin must be activated in project/plugins.sbt
EclipseKeys.withSource := true
