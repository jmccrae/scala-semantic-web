package scalasemweb.owl

import scala.collection._
import scalasemweb.rdf.model._
import scalasemweb.rdf.collection._

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// All different group

trait OWLDisjunctionGroup[Elem <: OWLEntity] extends View {
  
  protected def clazz : NamedNode
  
  protected def membersProp : NamedNode

  protected def make(resource : Resource, triples : TripleSet) : Elem
  
  protected def resource : Resource
  
  def members = {
    triples get(Some(resource),Some(membersProp),None) flatMap { 
      case _ %> _ %> (o : Resource) => {
        RDFList(o,triples) map {
          case cr : Resource => make(cr,triples)
          case lit => throw new OWLFormatException(lit + " was indicated as a member of an all disjoint list but is not a resource")
        }
      }
      case _ %> _ %> lit => throw new OWLFormatException(lit + " was indicated as a list of members of an all disjoint list but is not a resource")
    }
  }
  
  def frame : TripleSet = {
    val memLists = triples get(Some(resource),Some(membersProp),None)
    memLists + (resource %> RDF._type %> clazz) ++ 
    (memLists flatMap { 
      case x : Resource => RDFList(x,triples).frame
      case lit => throw new OWLFormatException(lit + " was indicated as a list of members of an all disjoint class list but is not a resource")
    }) 
  }
}

class OWLAllDisjointClasses private[owl] (val resource : Resource, val triples : TripleSet) extends OWLDisjunctionGroup[OWLClass]  {
  protected def membersProp = OWL.members
  protected def clazz = OWL.AllDisjointClasses
  protected def make(resource : Resource, triples : TripleSet) = OWLClass(resource,triples)
}

object OWLAllDisjointClasses {
  def apply(clazzes : OWLClass*) = {
    val n = AnonymousNode
    val l = RDFList((clazzes map { _.resource }):_*)
    new OWLAllDisjointClasses(n, TripleSet(n %> RDF._type %> OWL.AllDisjointClasses, 
      n %> OWL.members %> l.node) ++ l.triples)
  }
  
  def apply(resource : Resource, triples : TripleSet) = {
    if(triples has(Some(resource),Some(RDF._type),Some(OWL.AllDisjointClasses))) {
      new OWLAllDisjointClasses(resource,triples)
    } else {
      throw new OWLNoSuchEntityException()
    }
  }
}
 
class OWLAllDisjointProperties private[owl] (val resource : Resource, val triples : TripleSet) extends OWLDisjunctionGroup[OWLProperty[OWLType]]  {
  protected def membersProp = OWL.members
  protected def clazz = OWL.AllDisjointProperties
  protected def make(resource : Resource, triples : TripleSet) = OWLProperty(resource,triples)
}

object OWLAllDisjointProperties {
  def apply(clazzes : OWLProperty[OWLType]*) = {
    val n = AnonymousNode
    val l = RDFList((clazzes map { _.resource }):_*)
    new OWLAllDisjointClasses(n, TripleSet(n %> RDF._type %> OWL.AllDisjointProperties, 
      n %> OWL.members %> l.node) ++ l.triples)
  }
  
  def apply(resource : Resource, triples : TripleSet) = {
    if(triples has(Some(resource),Some(RDF._type),Some(OWL.AllDisjointProperties))) {
      new OWLAllDisjointClasses(resource,triples)
    } else {
      throw new OWLNoSuchEntityException()
    }
  }
}

class OWLAllDifferent private[owl] (val resource : Resource, val triples : TripleSet) extends OWLDisjunctionGroup[OWLIndividual]  {
  protected def membersProp = OWL.distinctMembers
  protected def clazz = OWL.AllDifferent
  protected def make(resource : Resource, triples : TripleSet) = OWLIndividual(resource,triples)
}

object OWLAllDifferent {
  def apply(clazzes : OWLIndividual*) = {
    val n = AnonymousNode
    val l = RDFList((clazzes map { _.resource }):_*)
    new OWLAllDisjointClasses(n, TripleSet(n %> RDF._type %> OWL.AllDifferent, 
      n %> OWL.distinctMembers %> l.node) ++ l.triples)
  }
  
  def apply(resource : Resource, triples : TripleSet) = {
    if(triples has(Some(resource),Some(RDF._type),Some(OWL.AllDifferent))) {
      new OWLAllDisjointClasses(resource,triples)
    } else {
      throw new OWLNoSuchEntityException()
    }
  }
}
