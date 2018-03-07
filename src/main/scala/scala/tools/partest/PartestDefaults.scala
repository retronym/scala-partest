package scala.tools
package partest

import scala.concurrent.duration.Duration
import scala.tools.nsc.Properties.{ propOrNone => prop }
import scala.util.Properties.jdkHome
import java.lang.Runtime.{ getRuntime => runtime }

object PartestDefaults {
  def sourcePath  = prop("partest.srcdir")      getOrElse "files"
  def javaCmd     = prop("partest.javacmd")     orElse    jdkexec("java")  getOrElse "java"
  def javacCmd    = prop("partest.javac_cmd")   orElse    jdkexec("javac") getOrElse "javac"
  def javaOpts    = prop("partest.java_opts")   getOrElse  ""     // opts when running java during tests
  def scalacOpts  = prop("partest.scalac_opts") getOrElse  ""

  def testBuild   = prop("partest.build")
  def errorCount  = prop("partest.errors")  map (_.toInt) getOrElse 0
  def numThreads  = math.max(1, prop("partest.threads") map (_.toInt) getOrElse runtime.availableProcessors)
  def execInProcess = {
    val prop = java.lang.Boolean.getBoolean("partest.exec.in.process")
    if (prop && numThreads > 1) throw new IllegalArgumentException("-Dpartest.exec.in.process may only be used with -Dpartest.threads=1")
    prop
  }
  def waitTime    = Duration(prop("partest.timeout") getOrElse "4 hours")

  //def timeout     = "1200000"   // per-test timeout

  // probe for the named executable
  private def jdkexec(name: String): Option[String] = {
    import scala.reflect.io.Path, Path._, scala.tools.nsc.Properties.isWin
    (Path(jdkHome) / "bin").walk.find(e => e.name == name || isWin && e.name.equalsIgnoreCase(s"$name.exe")).map(_.path)
  }
}
