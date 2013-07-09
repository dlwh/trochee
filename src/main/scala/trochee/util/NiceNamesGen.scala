package trochee.util

import reflect.SourceContext
import virtualization.lms.internal._
import virtualization.lms.internal.GenericFatCodegen
import virtualization.lms.internal.FatExpressions
import virtualization.lms.internal.Expressions

/**
 * 
 * @author dlwh
 */
trait NiceNamesGen extends GenericFatCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._

  override def quote(x: Exp[Any]): String = x match {
    case sym@Sym(x) => {
      sym.pos.collectFirst { case z if z.assignedVariable.nonEmpty => z.assignedVariable.get +"_"+x}.getOrElse(s"x$x")
    }
    case _ => super.quote(x)

  }

  val noLocalsForUpdates = new java.util.WeakHashMap[Sym[Any], String]

  def preferNoLocal(x: Exp[Any]): String = {
    var zz = noLocalsForUpdates.get(x)
    if (zz == null) {
      zz = quote(x)
    }
    zz
  }

  protected def cacheAndEmit(x: Sym[Any], exp: String) = {
    noLocalsForUpdates.put(x,exp)

    emitValDef(x, exp)
  }




  var tabWidth: Int = 0
  def addTab() = "  " * tabWidth


  def indent : String

  def addPos(sym: Sym[Any], hasPos: {def pos: SourceContext}) = {
    sym.withPos(List(hasPos.pos))
  }

}
