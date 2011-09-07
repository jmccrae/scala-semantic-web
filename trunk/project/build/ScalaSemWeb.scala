import sbt._
import com.weiglewilczek.bnd4sbt._

class ScalaSemWeb(info : ProjectInfo) extends DefaultProject(info) with BNDPlugin {
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
  override def outputPath = "target"
  
  override def bndBundleSymbolicName = "scalasemweb"
  override def bndBundleVersion = projectVersion.value.toString
  override def bndExportPackage = Seq("scalasemweb*")
  override def artifactBaseName = "scalasemweb-"+projectVersion.value
  override def bndImportPackage = Seq("*","scala","scala.*",
                                      "org.openrdf.model;resolution:=optional",
                                      "org.openrdf.repository;resolution:=optional")
  
  val sesameRepository = "Sesame Repository" at "http://repo.aduna-software.org/maven2/releases/"
  
  val sesameCore = "org.openrdf.sesame" % "sesame-model" % "2.3.2"
  val sesameRepo = "org.openrdf.sesame" % "sesame-repository-api" % "2.3.2"
  val sesameSailMemory = "org.openrdf.sesame" % "sesame-sail-memory" % "2.3.2" % "test"
  val sesameRepoSail = "org.openrdf.sesame" % "sesame-repository-sail" % "2.3.2" % "test"
  val slf4j = "org.slf4j" % "slf4j-api" % "1.5.5" % "test"
  val slf4jjdk = "org.slf4j" % "slf4j-jdk14" % "1.5.5" % "test"
  val scalacheck =  "org.scalatest" % "scalatest_2.8.0" % "1.3.1.RC2" % "test"
}
