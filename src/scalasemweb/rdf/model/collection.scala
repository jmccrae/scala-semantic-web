package scalasemweb.rdf.collection

import scalasemweb.rdf.model._
import scala.annotation._
import scala.collection.immutable._
import scala.collection.generic._
import scala.collection.mutable.Builder
import java.util.NoSuchElementException

/////////////////////////////////////////////////////////////////////////////////////////////
// Container

trait RDFContainer extends View with LinearSeq[Value] 

object RDFContainer {
  def apply(vals : Value*) : RDFContainer = {
    val n = AnonymousNode
    var viewStats = TripleSet.empty
    var i = 1
    for(value <- vals) {
      viewStats += n %> (RDF&("_"+i)) %> value
      i += 1
    }
    new RDFContainerIndex(1,n,viewStats,true)
  }
  
  def apply(node : Resource, statements : TripleSet) : RDFContainer = new RDFContainerIndex(1,node,statements,false)
  
  private lazy val _empty = new RDFContainerIndex(1,AnonymousNode,TripleSet.empty,true)
	def empty  : RDFContainer = _empty
	
	def newBuilder : Builder[Value,RDFContainer] = new Builder[Value,RDFContainer] {
	  private var tripSet = TripleSet.empty
	  private var index = 1
	  private val node = AnonymousNode
    def +=(value : Value) = {
      tripSet += (node %> (RDF&("_"+index)) %> value)
      index += 1
      this
    }
    def clear {
      tripSet = TripleSet.empty
      index = 1
    }
    def result : RDFContainer = new RDFContainerIndex(1,node,tripSet,true)
  }
	
	implicit def canBuildFrom : CanBuildFrom[RDFContainer,Value,RDFContainer] = 
	new CanBuildFrom[RDFContainer,Value,RDFContainer] {
	  def apply = newBuilder
	  def apply(stats : RDFContainer) = newBuilder
	}
}

private class RDFContainerIndex(index : Int, node : Resource, val statements : TripleSet, _exact : Boolean) extends RDFContainer {
  override def isEmpty = _head == None
  
  private lazy val _head = statements.get(Some(node),Some(RDF&("_"+index)),None) match {
    case TripleSet(_ %> _ %> head) =>  Some(head)
    case _ => None
  }
  override def head = _head match {
    case Some(h) => h
    case None =>  throw new NoSuchElementException("Head of empty container")
  }
  
  private lazy val _tail =  new RDFContainerIndex(index+1,node,statements,_exact)
  override def tail = if(isEmpty) {
    throw new NoSuchElementException("Tail of empty container")
  } else {
    _tail
  }
  
  def frame = if(_exact) {
    statements
  } else {
    var viewStats = TripleSet.empty
    var i = 0;
    while(statements.get(Some(node),Some(RDF&("_"+i)),None) match {
      case TripleSet(stat) => {
        viewStats += stat
        i += 1
        true
      }
      case _ => false
    }) { }
    viewStats
  }
  
  override def isExact = _exact
  
  def apply(i : Int) = statements.get(Some(node),Some(RDF&("_"+(i-index))),None) match {
    case TripleSet(_ %> _ %> value) => value
    case _ => throw new IndexOutOfBoundsException()
  }
  
  def length = if(isEmpty) { 0 } else { 1 + tail.length }
}

/////////////////////////////////////////////////////////////////////////////////////////////
// List

trait RDFList extends View with LinearSeq[Value] {
  def node : Resource
}

private class RDFNil(val statements : TripleSet = TripleSet.empty) extends RDFList {
  def node = RDF.nil
  def frame = TripleSet.empty
  override def isEmpty = true
  override def head = throw new NoSuchElementException("Head of empty list")
  override def tail = throw new NoSuchElementException("Tail of empty list")
  override def isExact = statements.isEmpty
  def apply(i : Int) = throw new IndexOutOfBoundsException
  def length = 0
}

private class RDFListNode(_node : Resource, val statements : TripleSet, exact : Boolean) extends RDFList {
  private lazy val _head = statements.get(Some(node),Some(RDF.first),None) match {
    case TripleSet(_ %> _ %> first) => first
    case _ => throw new RDFCollectionException("No or more than one first node")
  }
  override def head = _head
  private lazy val _tail = statements.get(Some(node),Some(RDF.rest),None) match {
    case TripleSet(_ %> _ %> rest) => rest match {
      case RDF.nil => new RDFNil(statements)
      case x : Resource => if(exact) {
        new RDFListNode(x,statements - (_node %> RDF.first %> _head) - (_node %> RDF.rest %> rest),true)
      } else {
        new RDFListNode(x,statements,false)
      }
      case _ => throw new RDFCollectionException("Rest node was a literal")
    }
    case _ => throw new RDFCollectionException("No or more than one rest node")
  }
  override def tail = _tail
  override def isEmpty = false
  def frame = if(exact) {
    statements
  } else {
    tail.frame + (node %> RDF.first %> head) + (node %> RDF.rest %> tail.node)
  }
  def node = _node
  override def isExact = exact
  def apply(i : Int) : Value = if(i == 0) {
    _head
  } else {
    _tail.apply(i-1)
  }
  def length : Int = 1 + _tail.length
}

object RDFList {
  def apply(vals : Value*) : RDFList =  if(vals.isEmpty) {
    new RDFNil(TripleSet.empty)
  } else {
    val f = vals.head
    val r = apply(vals.tail:_*)
    val n = AnonymousNode
    new RDFListNode(n,r.statements + (n %> RDF.first %> f) + (n %> RDF.rest %> r.node), true)
  }
  
  def apply(node : Resource, statements : TripleSet) : RDFList = node match {
    case RDF.nil => new RDFNil(statements)
    case res => new RDFListNode(res,statements,false)
  }
  
  def empty = new RDFNil(StatementSet.empty)
  
  def newBuilder : Builder[Value,RDFList] = new Builder[Value,RDFList] {
	  private var tripSet = TripleSet.empty
	  private var prev : Option[Resource] = None
	  private var head : Resource = RDF.nil
    def +=(value : Value) = {
      val node = AnonymousNode
      tripSet += (node %> RDF.first %> value)
      prev match {
        case Some(previous) =>  tripSet += (previous %> RDF.rest %> node)
        case None => head = node
      }
      this
    }
    def clear {
      tripSet = TripleSet.empty
      prev = None
      head = RDF.nil
    }
    def result : RDFList = head match {
      case RDF.nil => new RDFNil(tripSet)
      case h => new RDFListNode(head, tripSet + (prev.get %> RDF.rest %> RDF.nil),true)
    }
  }
	
	implicit def canBuildFrom : CanBuildFrom[RDFList,Value,RDFList] = 
	new CanBuildFrom[RDFList,Value,RDFList] {
	  def apply = newBuilder
	  def apply(stats : RDFList) = newBuilder
	}
}

class RDFCollectionException(message : String) extends RuntimeException(message)
