package trochee.codegen

import scala.virtualization.lms.internal._
import trochee.basic.{ExtraBaseExp, SimpleFieldsExp}
import trochee.util.NiceNamesGen
import scala.virtualization.lms.common.BaseExp

/**
 * TODO
 *
 * @author dlwh
 **/
trait ScalaSimpleFieldsGen extends ScalaFatCodegen with NiceNamesGen {

  override val IR: ExtraBaseExp with scala.virtualization.lms.internal.Expressions with scala.virtualization.lms.internal.Effects with scala.virtualization.lms.internal.FatExpressions with SimpleFieldsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@IR.FieldDeref(v, name) => emitValDef(addPos(sym, a), s"${quote(v)}.$name")
    case a@NewObject(args@_*) => emitValDef(addPos(sym, a), s"new ${a.tp}(${args.map(quote _).mkString(", ")})")
    case a@StaticMethodInvocation(name, meth, IndexedSeq(), args@_*) => emitValDef(addPos(sym, a), s"$name.$meth(${args.map(quote _).mkString(", ")})")
    case a@StaticMethodInvocation(name, meth, targs, args@_*) => emitValDef(addPos(sym, a), s"$name.$meth${targs.mkString("[", ", ", "]")}(${args.map(quote _).mkString(", ")})")
    case a@ArrayApply(x,n) => emitValDef(addPos(sym, a), "" + quote(x) + "(" + quote(n) + ")")
    case a@ArrayUpdate(x,n,y) => emitValDef(addPos(sym, a), quote(x) + "(" + quote(n) + ") = " + quote(y))
    case _ => super.emitNode(sym, rhs)
  }

  def indent: String = "  " * tabWidth
}
