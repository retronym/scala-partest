/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools.partest
package nest

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.{CompilerCommand, Global, Settings}
import scala.tools.nsc.reporters.{ConsoleReporter, Reporter}
import scala.reflect.io.{AbstractFile, NoAbstractFile}
import java.io.{FileWriter, PrintWriter}

import scala.tools.nsc.util.ClassPath
import scala.util.control.ControlThrowable

class ExtConsoleReporter(settings: Settings, val writer: PrintWriter) extends ConsoleReporter(settings, Console.in, writer) {
  shortname = true
  // override def error(pos: Position, msg: String): Unit
}

class TestSettings(cp: String, error: String => Unit) extends Settings(error) {
  @deprecated("Use primary constructor", "1.0.12")
  def this(cp: String) = this(cp, _ => ())
  nowarnings.value  = false
  encoding.value    = "UTF-8"
  classpath.value   = cp
}

class PartestGlobal(settings: Settings, reporter: Reporter) extends Global(settings, reporter) {
  // override def abort(msg: String): Nothing
  // override def globalError(msg: String): Unit
  // override def supplementErrorMessage(msg: String): String
}
object DirectCompiler {
  private val globalCache = new java.lang.ThreadLocal[PartestGlobal]()

}
class DirectCompiler(val runner: Runner) {
  def newGlobal(settings: Settings, reporter: Reporter): PartestGlobal = {
    def create(): PartestGlobal = {
      val g = new PartestGlobal(settings, reporter)
      DirectCompiler.globalCache.set(g)
      g
    }
    val newOutputDir = settings.outputDirs.getSingleOutput.get
    if (!PartestDefaults.reuseGlobal) create()
    else DirectCompiler.globalCache.get() match {
      case null => create()
      case cached =>
        // experimental mode of partest to reuse the symbol table for subsequent compilations
        val previousOutputDir = cached.settings.outputDirs.getSingleOutput.get
        cached.settings.outputDirs.setSingleOutput(newOutputDir)
        cached.settings.classpath.value = settings.classpath.value
        val plat = cached.platform
        val canMutateSettings = cached.settings.toString() == settings.toString
        if (canMutateSettings) {
          cached.currentSettings = settings
          plat.getClass.getMethod("currentClassPath_$eq", classOf[Option[_]]).invoke(plat, None)
          cached.reporter = reporter
          try {
            cached.invalidateClassPathEntries(previousOutputDir.file.getAbsolutePath)
//            resetSymbolTable(cached, newOutputDir.file)
            cached
          } catch {
            case _: DiscardGlobal =>
              create()
          }
        } else {
          create()
        }

    }
  }

  def newGlobal(settings: Settings, logWriter: FileWriter): Global =
    newGlobal(settings, new ExtConsoleReporter(settings, new PrintWriter(logWriter)))

  private class DiscardGlobal extends ControlThrowable
//  private def resetSymbolTable(g: Global, outputDir: File): Unit = {
//    import g._
//    def walkTopLevels(root: Symbol): Unit = {
//      def safeInfo(sym: Symbol): Type =
//        if (sym.hasRawInfo && sym.rawInfo.isComplete) sym.info else NoType
//      def packageClassOrSelf(sym: Symbol): Symbol =
//        if (sym.hasPackageFlag && !sym.isModuleClass) sym.moduleClass else sym
//
//      val reset = collection.mutable.Set[Symbol]()
//      for (x <- safeInfo(packageClassOrSelf(root)).decls) {
//        if (x == root) ()
//        else if (x.hasPackageFlag && x != rootMirror.EmptyPackage) {
//          x.moduleClass.rawInfo match {
//            case l: loaders.PackageLoader =>
//              x.moduleClass.setInfo(new loaders.PackageLoader(x.fullNameString, g.classPath))
//              x.setInfo(x.moduleClass.tpe)
//            case _ =>
//              walkTopLevels(x)
//          }
//        } else if (x.owner != root) { // exclude package class members
//          var sym = x.enclosingTopLevelClass
//          if (x.isModule) sym = x.moduleClass
//          if (sym.isModuleClass) sym = sym.companionClass.orElse(sym)
//          if (sym.hasRawInfo) {
//            val assocFile = if (sym.rawInfo.isComplete)
//              sym.associatedFile
//            else sym.rawInfo match {
//              case l: loaders.ClassfileLoader =>
//                l.classfile
//              case _ =>
//                NoAbstractFile
//            }
//            if (assocFile != NoAbstractFile) {
//              val fromJar = assocFile.path.endsWith(".class") && assocFile.underlyingSource.isDefined
//              if (!fromJar) {
//                if (sym.hasTransOwner(definitions.ScalaPackageClass)) throw new DiscardGlobal
//                sym.owner.ownerChain.takeWhile(!_.isEffectiveRoot).foreach { owner =>
//                  if (!reset.contains(owner)) {
//                    //                  println("reset: " + owner)
//                    owner.setInfo(new loaders.PackageLoader(owner.fullNameString, classPath))
//                    owner.sourceModule.setInfo(owner.tpe)
//                    reset += owner
//                  }
//                }
//              }
//            }
//          }
//        }
//      }
//    }
//    exitingTyper {
//      import rootMirror._
//      EmptyPackageClass.setInfo(new loaders.PackageLoader(ClassPath.RootPackage, classPath))
//      EmptyPackage setInfo EmptyPackageClass.tpe
//      walkTopLevels(RootClass)
//      val oldRootDecls = RootClass.tpe.decls
//      RootClass.setInfo(new loaders.PackageLoader(ClassPath.RootPackage, classPath))
//      RootClass.initialize
//      RootPackage.setInfo(NullaryMethodType(RootClass.tpe))
//      val newRootDecls = RootClass.tpe.decls
//      newRootDecls.toList.foreach { oldPack =>
//        if (oldRootDecls.containsName(oldPack.name)) {
//          newRootDecls.lookup(oldPack.name) match {
//            case NoSymbol =>
//            case sym =>
//              newRootDecls.unlink(sym)
//              newRootDecls.enter(oldPack)
//          }
//        }
//      }
//      this.getClass
//    }
//  }


  /** Massage args to merge plugins and fix paths.
   *  Plugin path can be relative to test root, or cwd is out.
   *  While we're at it, mix in the baseline options, too.
   *  That's how ant passes in the plugins dir.
   */
  private def updatePluginPath(args: List[String], out: AbstractFile, srcdir: AbstractFile): Seq[String] = {
    val dir = PathSettings.testRoot
    // The given path, or the output dir if ".", or a temp dir if output is virtual (since plugin loading doesn't like virtual)
    def pathOrCwd(p: String) =
      if (p == ".") {
        val plugxml = "scalac-plugin.xml"
        val pout = if (out.isVirtual) Directory.makeTemp() else Path(out.path)
        val srcpath = Path(srcdir.path)
        val pd = (srcpath / plugxml).toFile
        if (pd.exists) pd copyTo (pout / plugxml)
        pout.toAbsolute
      } else Path(p)
    def absolutize(path: String) = pathOrCwd(path) match {
      case x if x.isAbsolute => x.path
      case x                 => (dir / x).toAbsolute.path
    }

    val xprefix = "-Xplugin:"
    val (xplugs, others) = args partition (_ startsWith xprefix)
    val Xplugin = if (xplugs.isEmpty) Nil else List(xprefix +
      (xplugs map (_ stripPrefix xprefix) flatMap (_ split pathSeparator) map absolutize mkString pathSeparator)
    )
    runner.suiteRunner.scalacExtraArgs ++ runner.suiteRunner.scalacOpts.split(' ') ++ others ++ Xplugin
  }

  def compile(opts0: List[String], sources: List[File]): TestState = {
    import runner.{ sources => _, _ }

    // adding codelib.jar to the classpath
    // codelib provides the possibility to override standard reify
    // this shields the massive amount of reification tests from changes in the API
    val codeLib = PathSettings.srcCodeLib.fold[List[Path]](x => Nil, lib => List[Path](lib))
    // add the instrumented library version to classpath -- must come first
    val specializedOverride: List[Path] = if (kind == "specialized") List(PathSettings.srcSpecLib.fold(sys.error, identity)) else Nil

    val classPath: List[Path] = specializedOverride ++ codeLib ++ fileManager.testClassPath ++ List[Path](outDir)

    val parseArgErrors = ListBuffer.empty[String]

    val testSettings = new TestSettings(FileManager.joinPaths(classPath), s => parseArgErrors += s)
    val logWriter    = new FileWriter(logFile)
    val srcDir       = if (testFile.isDirectory) testFile else Path(testFile).parent.jfile
    val opts         = updatePluginPath(opts0, AbstractFile getDirectory outDir, AbstractFile getDirectory srcDir)
    val command      = new CompilerCommand(opts.toList, testSettings)
    testSettings.outputDirs setSingleOutput outDir.getPath
    val global       = newGlobal(testSettings, logWriter)
    val reporter     = global.reporter.asInstanceOf[ExtConsoleReporter]
    def errorCount   = reporter.errorCount


    def reportError(s: String): Unit = reporter.error(null, s)

    parseArgErrors.toList foreach reportError

    // check that option processing succeeded
    if (opts0.nonEmpty) {
      if (!command.ok) reportError(opts0.mkString("bad options: ", space, ""))
      if (command.files.nonEmpty) reportError(command.files.mkString("flags file may only contain compiler options, found: ", space, ""))
    }

    def ids = sources.map(_.testIdent) mkString space
    nestUI.vlog(s"% scalac $ids")

    def execCompile() =
      if (command.shouldStopWithInfo) {
        logWriter append (command getInfoMessage global)
        runner genFail "compilation stopped with info"
      } else {
        new global.Run compile sources.map(_.getPath)
        if (!reporter.hasErrors) runner.genPass()
        else {
          reporter.printSummary()
          reporter.writer.close()
          runner.genFail(s"compilation failed with $errorCount errors")
        }
      }

    try     { execCompile() }
    catch   { case t: Throwable => reportError(t.getMessage) ; runner.genCrash(t) }
    finally { logWriter.close() }
  }
}
