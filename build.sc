//| mill-version: 1.0.5
// build.sc
import mill.*
import mill.api.*
import scalalib.*
import publish.*

object scas extends ScalaModule with PublishModule {
  def scalaVersion = sys.props("dottyVersion")
  def mvnDeps = Seq(
    mvn"org.scala-lang.modules:scala-parallel-collections_3:1.0.4"
  )
  object application extends ScalaModule {
    def scalaVersion = sys.props("dottyVersion")
    def moduleDeps = Seq(scas)
    def mvnDeps = Seq(
      mvn"org.scala-lang::scala3-compiler:${scalaVersion()}",
      mvn"de.uni-mannheim.rz.krum:jas:2.7.200",
      mvn"org.apache.logging.log4j:log4j-core:2.24.3",
      mvn"org.apache.logging.log4j:log4j-api:2.24.3",
      mvn"cc.redberry:rings:2.5.7",
      mvn"org.apache.commons:commons-math3:3.6.1",
      mvn"net.sourceforge.jscl-meditor:rendering:1.1",
      mvn"org.scala-lang.modules:scala-parser-combinators_3:2.4.0"
    )
    def test(args: String*) = run(Task.Anon(Args(args)))
  }
  def publishVersion = "3.1"

  def pomSettings = PomSettings(
    description = "Scala Algebra System",
    organization = "com.github.rjolly",
    url = "https://github.com/rjolly/scas",
    licenses = Seq(License.`LGPL-2.0+`),
    versionControl = VersionControl.github("rjolly", "scas"),
    developers = Seq(
      Developer("rjolly", "Raphael Jolly", "https://github.com/rjolly")
    )
  )
}
