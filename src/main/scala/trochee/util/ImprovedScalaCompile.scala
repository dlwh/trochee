package trochee.util

import java.io.{PrintWriter, StringWriter}
import reflect.io.VirtualDirectory
import tools.nsc.util
import tools.nsc.interpreter.AbstractFileClassLoader
import virtualization.lms.internal.ScalaCompile

/**
 * 
 * @author dlwh
 */
trait ImprovedScalaCompile extends ScalaCompile {
  def compile2[A:Manifest, A2: Manifest, B: Manifest](f: (Exp[A], Exp[A2]) => Exp[B]): (A, A2)=>B = {
    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1

    val source = new StringWriter()
    val staticData = codegen.emitSource2(f, className, new PrintWriter(source))

    if (dumpGeneratedCode) println(source)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
  //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)

    val obj: (A,A2)=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[(A, A2)=>B]
    obj
  }
}
