package trochee.parser

import virtualization.lms.common.{RangeOps, Base}
import trochee.kernels.KernelOps
import spire.implicits._
import spire.syntax._
import spire.math._
import trochee.basic.SpireOps

/**
 * 
 * @author dlwh
 */
trait InliningInsideKernels extends InsideKernels { self: Base with KernelOps with RangeOps with SpireOps =>
  protected def doInsideUnaries(top: Rep[ParseCell], bot: Rep[ParseCell], grammar: Rep[Int], rules: Rep[RuleCell]): Rep[Unit] = {
    for( (parent, rr) <- this.grammar.unaryRules.groupBy(_._1.parent)) {
      for((r,id) <- rr) {
        val topSym = top.syms(parent).apply(grammar)
        val botScore = bot.syms(r.child) apply grammar
        top.syms(parent)(grammar) = (topSym + botScore * rules.rules(id)(grammar))
        unitToRepUnit()
      }

      unitToRepUnit()

    }
    unitToRepUnit()
  }
}
