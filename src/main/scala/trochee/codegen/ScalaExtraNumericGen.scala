package trochee.codegen

import scala.virtualization.lms.internal._
import trochee.basic._
import trochee.util.NiceNamesGen
import scala.virtualization.lms.common.BaseExp

/**
 * TODO
 *
 * @author dlwh
 **/
trait ScalaExtraNumericGen extends ScalaFatCodegen with NiceNamesGen {

  override val IR: scala.virtualization.lms.internal.Expressions with scala.virtualization.lms.internal.Effects with scala.virtualization.lms.internal.FatExpressions with ExtraBaseExp with ExtraNumericOpsExp

  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ExtraNumericMod(x,y) => emitValDef(addPos(sym, a), s"${quote(x)} % ${quote(y)}")
    case a@ExtraNumericPow(x,y) if a.manifest == Manifest.Int => emitValDef(addPos(sym, a), s"breeze.numerics.IntMath.pow(${quote(x)}, ${quote(y)})")
    case a@ExtraNumericPow(x,y) => emitValDef(addPos(sym, a), s"scala.math.pow(${quote(x)}, ${quote(y)})")
    case _ => super.emitNode(sym, rhs)
  }

}
