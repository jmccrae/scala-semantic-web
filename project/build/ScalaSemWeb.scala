import sbt._
import com.weiglewilczek.bnd4sbt._

class ScalaSemWeb(info : ProjectInfo) extends DefaultProject(info) with BNDPlugin {
  override def compileOptions = super.compileOptions ++ Seq(Unchecked)
  override def outputPath = "target"
  
  override def bndBundleSymbolicName = "scalasemweb"
  override def bndBundleVersion = projectVersion.value.toString
  override def bndExportPackage = Seq("scalasemweb*")
  override def artifactBaseName = "scalasemweb-"+projectVersion.value
}
