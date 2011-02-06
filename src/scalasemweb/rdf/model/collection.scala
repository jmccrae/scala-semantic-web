package scalasemweb.rdf.collection

import scalasemweb.rdf.model._
import scala.annotation._
import scala.collection.LinearSeqLike
import scala.collection.immutable.{LinearSeq}
import scala.collection.generic._
import scala.collection.mutable.Builder
import java.util.NoSuchElementException

/////////////////////////////////////////////////////////////////////////////////////////////
// Container

trait RDFContainer extends View with LinearSeq[Value] with LinearSeqLike[Value,RDFContainer] {
  protected[this] override def newBuilder = RDFContainer.newBuilder
}

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
  
  def apply(node : Resource, triples : TripleSet) : RDFContainer = new RDFContainerIndex(1,node,triples,false)
  
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

private class RDFContainerIndex(index : Int, node : Resource, val triples : TripleSet, _exact : Boolean) extends RDFContainer {
  override def isEmpty = _head == None
  
  private lazy val _head = triples.get(Some(node),Some(RDF&("_"+index)),None) match {
    case TripleSet(_ %> _ %> head) =>  Some(head)
    case _ => None
  }
  override def head = _head match {
    case Some(h) => h
    case None =>  throw new NoSuchElementException("Head of empty container")
  }
  
  private lazy val _tail =  new RDFContainerIndex(index+1,node,triples,_exact)
  override def tail = if(isEmpty) {
    throw new NoSuchElementException("Tail of empty container")
  } else {
    _tail
  }
  
  private lazy val _frame = if(_exact) {
    triples
  } else {
    var viewStats = TripleSet.empty
    var i = 0;
    while(triples.get(Some(node),Some(RDF&("_"+i)),None) match {
      case TripleSet(stat) => {
        viewStats += stat
        i += 1
        true
      }
      case _ => false
    }) { }
    viewStats
  }
  def frame = _frame
  
  override def isExact = _exact
  
  def apply(i : Int) = triples.get(Some(node),Some(RDF&("_"+(i-index))),None) match {
    case TripleSet(_ %> _ %> value) => value
    case _ => throw new IndexOutOfBoundsException()
  }
  
  def length = if(isEmpty) { 0 } else { 1 + tail.length }
}

/////////////////////////////////////////////////////////////////////////////////////////////
// List

trait RDFList extends View with LinearSeq[Value] with LinearSeqLike[Value,RDFList] {
  def node : Resource
  protected[this] override def newBuilder = RDFList.newBuilder
}

private class RDFNil(val triples : TripleSet = TripleSet.empty) extends RDFList {
  def node = RDF.nil
  def frame = TripleSet.empty
  override def isEmpty = true
  override def head = throw new NoSuchElementException("Head of empty list")
  override def tail = throw new NoSuchElementException("Tail of empty list")
  override def isExact = triples.isEmpty
  def apply(i : Int) = throw new IndexOutOfBoundsException
  def length = 0
}

private class RDFListNode(_node : Resource, val triples : TripleSet, exact : Boolean) extends RDFList {
  private lazy val _head = triples.get(Some(node),Some(RDF.first),None) match {
    case TripleSet(_ %> _ %> first) => first
    case _ => throw new RDFCollectionException("No or more than one first node")
  }
  override def head = _head
  private lazy val _tail = triples.get(Some(node),Some(RDF.rest),None) match {
    case TripleSet(_ %> _ %> rest) => rest match {
      case RDF.nil => new RDFNil(triples)
      case x : Resource => if(exact) {
        new RDFListNode(x,triples - (_node %> RDF.first %> _head) - (_node %> RDF.rest %> rest),true)
      } else {
        new RDFListNode(x,triples,false)
      }
      case _ => throw new RDFCollectionException("Rest node was a literal")
    }
    case _ => throw new RDFCollectionException("No or more than one rest node")
  }
  override def tail = _tail
  override def isEmpty = false
  private lazy val _frame = if(exact) {
    triples
  } else {
    tail.frame + (node %> RDF.first %> head) + (node %> RDF.rest %> tail.node)
  }
  def frame = _frame
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
  def apply(vals : Value*) : RDFList = fromSeq(vals)  
  
  def apply(node : Resource, triples : TripleSet) : RDFList = node match {
    case RDF.nil => new RDFNil(triples)
    case res => new RDFListNode(res,triples,false)
  }
  
  def fromSeq(vals : Iterable[Value]) : RDFList = if(vals.isEmpty) {
    new RDFNil(TripleSet.empty)
  } else {
    val f = vals.head
    val r = fromSeq(vals.tail)
    val n = AnonymousNode
    new RDFListNode(n,r.triples + (n %> RDF.first %> f) + (n %> RDF.rest %> r.node), true)
  }
  
  def empty : RDFList = new RDFNil(TripleSet.empty)
  
  def newBuilder : Builder[Value,RDFList] = new Builder[Value,RDFList] {
	  private var tripSet = TripleSet.empty
	  private var head : Resource = RDF.nil
	  private var prev : Option[Resource] = None
    def +=(value : Value) = {
      val node = AnonymousNode
      tripSet += (node %> RDF.first %> value)
      prev match {
        case None => head = node 
        case Some(previous) => tripSet += (previous %> RDF.rest %> node)
      }
      prev = Some(node)
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
