package trochee.ops

/**
 * 
 * @author dlwh
 */
trait IndexOp[V, I, +R] {
  def apply(v: V, i: I):R

}
