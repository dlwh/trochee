package trochee.parser

/**
 * 
 * @author dlwh
 */
trait Grammar {
  type Real

  def numSyms: Int

  def ruleScores: Array[Real]

  def leftTermRules: IndexedSeq[(BinaryRule[Int], Int)]
  def rightTermRules: IndexedSeq[(BinaryRule[Int], Int)]
  def nontermRules: IndexedSeq[(BinaryRule[Int], Int)]
  def bothTermRules: IndexedSeq[(BinaryRule[Int], Int)]

  def unaryRules: IndexedSeq[(UnaryRule[Int], Int)]
}

sealed trait Rule[+L]
case class BinaryRule[+L](parent: L, leftChild: L, rightChild: L) extends Rule[L]
case class UnaryRule[+L](parent: L, child: L) extends Rule[L]
