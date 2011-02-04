package scalasemweb.rdf.model.aux

import scalasemweb.rdf.model._

trait RDFPair[+T <: Resource, +U <: Value] {
	def _1 : T
	def _2 : U
}

case class PredObj(val predicate : NamedNode, val obj : Value) extends RDFPair[NamedNode,Value] {
	def _1 = predicate
	def _2 = obj
}

