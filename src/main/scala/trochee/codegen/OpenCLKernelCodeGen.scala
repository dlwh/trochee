package trochee.codegen

import virtualization.lms.internal.{Effects, FatExpressions, GenericCodegen, Expressions}
import java.io.{StringWriter, PrintWriter}
import trochee.kernels.{KernelOpsExp, KernelOps}
import virtualization.lms.common.{OrderingOpsExp, IfThenElseExp}
import trochee.util.NiceNamesGen

/**
 * 
 * @author dlwh
 */
trait OpenCLKernelCodegen extends GenericCodegen with OpenCLKernelGenBase with OpenCLKernelGenOrderingOps with OpenCLKernelGenIfThenElseOps with NiceNamesGen {
  val IR: Expressions with Effects with FatExpressions with KernelOpsExp with OrderingOpsExp with IfThenElseExp
  import IR._

  private val headerPieces = collection.mutable.ArrayBuffer[String]()
  protected def writeHeader() {
    for(piece <- headerPieces) {
      stream.println(piece)
    }
  }

  trait CStruct extends Struct[Exp]
  case class CStructSpec[T:Manifest](args: IndexedSeq[(String, Manifest[_])]) extends Rep[T] {
    def rep = {
      tabWidth += 1
      val ret = args.map{case (x,tpe) => remap(tpe) + " " + x +";"}.mkString("struct {","\n" + indent, "}")
      tabWidth -= 1
      ret
    }
  }
  def __new[T:Manifest](args: (String, Boolean, Exp[T] => Rep[_])*) = new CStructSpec(args.map{ case (name, _, rhs) => name -> rhs(fresh[T]).tp }.toIndexedSeq)

  def define[T<:AnyVal](name: String, value: T) { headerPieces += s"#define $name $value" }
  def define(name: String, value: String) { headerPieces += s"#define $name $value" }
  def struct(name: String, value: Rep[CStruct]) { headerPieces += s"typedef ${value.asInstanceOf[CStructSpec[_]].rep} $name;" }

  def indent: String = "  " * tabWidth

  private var _registered = false

  def register() {}

  def mkKernel(k: Kernel): String = {
    (this) synchronized {
      if(!_registered) {
        _registered = true
        register()
      }
    }
    import k._
    val sout = new StringWriter()
    val out = new PrintWriter(sout)
    withStream(out) {
      writeHeader()
      stream.println(s"__kernel void $name("+args.zip(qualifiers).map{ case (a, q) => q + " " + remap(a.tp) + " " + quote(a)}.mkString(", ")+") {")
      tabWidth += 1
      emitBlock(fn)
      //stream.println(quote(getBlockResult(y)))
      tabWidth -= 1

      stream.println("}")
    }

    val ret = sout.toString
    println(ret)
    ret
  }

  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {

    withStream(out) {
      // TODO: add kernel spec!
      stream.println(s"__kernel void $className("+args.map(a => remap(a.tp) + " " + quote(a)).mkString(", ")+") {")
      tabWidth += 1
      emitBlock(body)
      //stream.println(quote(getBlockResult(y)))
      tabWidth -= 1

      stream.println("}")
    }
    Nil
  }


  def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if(remap(sym.tp) == "void")
      stream.println(addTab() + rhs + ";")
    else
      stream.println(addTab() + remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.println(addTab()+ remap(sym.tp) + " " + quote(sym) + " = " + rhs + ";")
  }

  def emitAssignment(lhs:String, rhs: String): Unit = {
    stream.println(addTab() + " " + lhs + " = " + rhs + ";")
  }


  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[AnyVal]]) {
      remap(m.typeArguments.head)
    } else if (m.erasure.isArray) {
      remap(new Manifest[Any] {
        def runtimeClass = m.erasure.getComponentType
      }) + "*"
    } else {
      m.toString.toLowerCase match {
          case "boolean" => "bool"
          case "byte" => "char"
          case "char" => "char"
          case "short" => "short"
          case "int" => "int"
          case "long" => "long"
          case "float" => "float"
          case "double" => "double"
          case "unit"  => "void"
          case _ => throw new Exception("GPUGen: remap(m) : GPUable Type %s does not have mapping table.".format(m.toString))
      }
    }
  }
}
