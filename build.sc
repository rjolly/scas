//| mill-version: 0.10.15
// build.sc
import mill._, scalalib._, publish._

object scas extends ScalaModule with PublishModule {
  def scalaVersion = sys.props("dottyVersion")
  def ivyDeps = Agg(
    ivy"org.scala-lang.modules:scala-parallel-collections_3:1.0.4"
  )
  object application extends ScalaModule with PublishModule {
    def publishVersion = scas.publishVersion
    def pomSettings = scas.pomSettings
    override def artifactName = "scas.application"
    def scalaVersion = sys.props("dottyVersion")
    def moduleDeps = Seq(scas)
    def ivyDeps = Agg(
      ivy"org.scala-lang::scala3-repl:${scalaVersion()}",
      ivy"de.uni-mannheim.rz.krum:jas:2.7.200",
      ivy"org.apache.logging.log4j:log4j-core:2.24.3",
      ivy"org.apache.logging.log4j:log4j-api:2.24.3",
      ivy"cc.redberry:rings:2.5.7",
      ivy"org.apache.commons:commons-math3:3.6.1",
      ivy"net.sourceforge.jscl-meditor:rendering:1.1",
      ivy"org.scala-lang.modules:scala-parser-combinators_3:2.4.0"
    )
    def test(args: String*) = run(args: _*)
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
