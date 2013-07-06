package trochee.codegen.opencl

import virtualization.lms.common._
import virtualization.lms.internal.{FatExpressions, Effects, Expressions}
import trochee.kernels.KernelOpsExp

/**
  *
  * @author dlwh
  */
trait OpenCLKernelGenIfThenElseOps extends OpenCLKernelGenBase {
  val IR : Expressions with Effects with FatExpressions with IfThenElseExp with KernelOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
     rhs match {
       case IfThenElse(c,a,b) =>
         //TODO: using if-else does not work
         remap(sym.tp) match {
           case "void" =>
             stream.println(indent + "if (" + quote(c) + ") {")
             tabWidth += 1
             emitBlock(a)
             tabWidth -= 1
             stream.println(indent+"} else {")
             tabWidth += 1
             emitBlock(b)
             tabWidth -= 1
             stream.println(indent+"}")
           case _ =>
             stream.println("%s %s;".format(remap(sym.tp),quote(sym)))
             stream.println(indent+"if (" + quote(c) + ") {")
             tabWidth += 1
             emitBlock(a)
             tabWidth -= 1
             stream.println(indent+"%s = %s;".format(quote(sym),quote(getBlockResult(a))))
             stream.println(indent+"} else {")
             tabWidth += 1
             emitBlock(b)
             tabWidth -= 1
             stream.println(indent+"%s = %s;".format(quote(sym),quote(getBlockResult(b))))
             stream.println(indent+"}")
         }
         /*
         val booll = remap(sym.tp).equals("void")
         if(booll) {
           stream.println("%s %s;".format(remap(sym.tp),quote(sym)))
           stream.println("if (" + quote(c) + ") {")
           emitBlock(a)
           stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(a))))
           stream.println("} else {")
           emitBlock(b)
           stream.println("%s = %s;".format(quote(sym),quote(getBlockResult(b))))
           stream.println("}")
         }
         else {
           stream.println("if (" + quote(c) + ") {")
           emitBlock(a)
           stream.println("} else {")
           emitBlock(b)
           stream.println("}")
         }
         */
       case _ => super.emitNode(sym, rhs)
     }
   }
 }
