// build.sc
import mill._, scalalib._, publish._

object scas extends ScalaModule with PublishModule {
  def scalaVersion = sys.props("dottyVersion")
  def scalacOptions = Seq("-language:experimental.genericNumberLiterals")
  def ivyDeps = Agg(
    ivy"org.scala-lang:scala-library:2.13.4"
  )
  object application extends ScalaModule {
    def scalaVersion = sys.props("dottyVersion")
    def scalacOptions = Seq("-language:experimental.genericNumberLiterals")
    def moduleDeps = Seq(scas)
    def ivyDeps = Agg(
      ivy"org.scala-lang::scala3-compiler:${scalaVersion()}",
      ivy"de.uni-mannheim.rz.krum:jas:2.7.20",
      ivy"cc.redberry:rings:2.5.7",
      ivy"org.apache.commons:commons-math3:3.6.1",
      ivy"net.sourceforge.jscl-meditor:rendering:1.0"
    )
    def test(args: String*) = run(args: _*)
  }
  def publishVersion = "3.0"

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

  override def docJar = T {
    val outDir = T.dest
    val javadocDir = outDir / 'javadoc
    os.makeDir.all(javadocDir)
    eval.Result.Success(modules.Jvm.createJar(Agg(javadocDir))(outDir))
  }
}
