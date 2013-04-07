package trochee.codegen

import virtualization.lms.internal.{FatExpressions, Effects, Expressions}
import trochee.kernels.KernelOpsExp
import java.util
import trochee.parser.ExtraBaseExp

/**
 * 
 * @author dlwh
 */
trait OpenCLKernelGenArrayOps extends OpenCLKernelGenBase {
  val IR:  Expressions with Effects with FatExpressions with ExtraBaseExp with KernelOpsExp
  import IR._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n) => remap(a.m) + " " + quote(addPos(sym,a)) + "[" + quote(n) +"];"
    case a@ArrayApply(x,n) =>
      val str = "" + preferNoLocal(x) + "[" + quote(n) + "]"
      cacheAndEmit(addPos(sym, a), str)
    case ArrayUpdate(x,n,y) =>
      val zz: String = preferNoLocal(x)
      stream.println(indent + zz + "[" + quote(n) + "] = " + quote(y) + ";")
    case _ => super.emitNode(sym, rhs)
  }


}
