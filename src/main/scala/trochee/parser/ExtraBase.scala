package trochee.parser

import virtualization.lms.common._
import reflect.SourceContext
import virtualization.lms.internal.Effects
import virtualization.lms.common.BaseExp
import virtualization.lms.common.Variables
import virtualization.lms.common.Base

/**
 *  A lot of duplication from LMS... but...
 * @author dlwh
 */
trait ExtraBase { this: Base with Variables =>

  implicit def varToArrayOps[T:Manifest](x: Var[Array[T]]) = new ArrayOpsCls(readVar(x))
  implicit def repArrayToArrayOps[T:Manifest](a: Rep[Array[T]]) = new ArrayOpsCls(a)
  implicit def arrayToArrayOps[T:Manifest](a: Array[T]) = new ArrayOpsCls(unit(a))

  // substitution for "new Array[T](...)"
  // TODO: look into overriding __new for arrays
  object NewArray {
    def apply[T:Manifest](n: Rep[Int])(implicit pos: SourceContext)= array_obj_new(n)(implicitly[Manifest[T]], pos)
  }

  class ArrayOpsCls[T:Manifest](a: Rep[Array[T]]){
    def apply(n: Rep[Int])(implicit pos: SourceContext) = array_apply(a, n)(implicitly, pos)
    def update(n: Rep[Int], y: Rep[T])(implicit pos: SourceContext) = {
      array_update(a,n,y)(implicitly, pos)
    }
  }

  def array_obj_new[T:Manifest](n: Rep[Int])(implicit pos: SourceContext): Rep[Array[T]]
  def array_apply[T:Manifest](x: Rep[Array[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T]
  def array_update[T:Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Unit]

  object Arrays {
    def fill[T: Manifest](arr: Rep[Array[T]], length: Rep[Int], value: Rep[T])(implicit pos: SourceContext):Rep[Unit] = {
      arrays_fill(arr, length, value)(implicitly, pos)
    }
  }

  def arrays_fill[T:Manifest](x: Rep[Array[T]], length: Rep[Int], n: Rep[T])(implicit pos: SourceContext): Rep[Unit]


}

trait ExtraBaseExp extends ExtraBase with EffectExp with VariablesExp { this: BaseExp with Variables =>
  case class ArrayNew[T:Manifest](n: Exp[Int])(implicit val pos: SourceContext) extends Def[Array[T]] {
    val m = manifest[T]
  }
  case class ArrayApply[T:Manifest](a: Exp[Array[T]], n: Exp[Int])(implicit val pos: SourceContext) extends Def[T]
  case class ArrayUpdate[T:Manifest](a: Exp[Array[T]], n: Exp[Int], y: Exp[T])(implicit val pos: SourceContext) extends Def[Unit]
  def array_obj_new[T: Manifest](n: Rep[Int])(implicit pos: SourceContext): Rep[Array[T]] = reflectMutable(ArrayNew(n)(implicitly, pos))

  def array_apply[T: Manifest](x: Rep[Array[T]], n: Rep[Int])(implicit pos: SourceContext): Rep[T] = ArrayApply(x,n)(implicitly, pos)

  def array_update[T: Manifest](x: Rep[Array[T]], n: Rep[Int], y: Rep[T])(implicit pos: SourceContext): Rep[Unit] = {
    reflectWrite(x)(ArrayUpdate(x,n,y)(implicitly, pos))
  }

  def arrays_fill[T: Manifest](x: Rep[Array[T]], length: Rep[Int], n: Rep[T])(implicit pos: SourceContext): Rep[Unit] = ???

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case ArrayApply(a,x) => array_apply(f(a),f(x))(mtype(manifest[A]),pos)
    case Reflect(e@ArrayNew(n), u, es) => reflectMirrored(Reflect(ArrayNew(f(n))(e.m, e.pos), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@ArrayApply(l,r), u, es) => reflectMirrored(Reflect(ArrayApply(f(l),f(r))(mtype(manifest[A]), e.pos), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@ArrayUpdate(l,i,r), u, es) => reflectMirrored(Reflect(ArrayUpdate(f(l),f(i),f(r))(implicitly, e.pos), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??

}
