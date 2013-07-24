package trochee.codegen

import scala.virtualization.lms.internal._
import trochee.basic._
import trochee.basic.collection._
import trochee.util.NiceNamesGen
import scala.virtualization.lms.common.BaseExp

/**
 * TODO
 *
 * @author dlwh
 **/
trait ScalaBitSetGen extends ScalaFatCodegen with NiceNamesGen {

  override val IR: scala.virtualization.lms.internal.Expressions with scala.virtualization.lms.internal.Effects with scala.virtualization.lms.internal.FatExpressions with ExtraBaseExp with BitSetOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@BitSet_+=(x,y) => emitValDef(addPos(sym, a), s"${quote(x)} += ${quote(y)}")
    case a@BitSet_apply(x,y) => emitValDef(addPos(sym, a), s"${quote(x)}.contains(${quote(y)})")
    case a:NewBitSet =>  emitValDef(addPos(sym, a), s"new java.util.BitSet()")
    case _ => super.emitNode(sym, rhs)
  }

}
