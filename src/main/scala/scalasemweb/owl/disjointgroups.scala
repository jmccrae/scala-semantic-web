package scalasemweb.owl

import scala.collection._
import scalasemweb.rdf.model._
import scalasemweb.rdf.collection._

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// All different group

/** A group of disjunct elements */
trait OWLDisjunctionGroup[Elem <: OWLEntity] extends View {
  
  protected def clazz : NamedNode
  
  protected def membersProp : NamedNode

  protected def make(resource : Resource, triples : TripleSet) : Elem
  
  protected def resource : Resource
  
  /** Get all members of this disjunct group */
  def members : Set[Elem] = {
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
  
  override def equals(obj : Any) = obj match {
    case x : OWLDisjunctionGroup[_] => members == x.members
    case _ => false
  }
  
  override def hashCode = members.hashCode
}

object OWLDisjunctionGroup {
  /** Make a disjoint group at the given resource.
   * @throws OWLNoSuchEntityException If there is no disjoint group at this URI
   */
  def apply(resource : Resource, triples : TripleSet) = {
    triples.get(Some(resource),Some(RDF._type),None).view.map {
      case _ %> _ %> OWL.AllDisjointClasses => Some(new OWLAllDisjointClasses(resource,triples))
      case _ %> _ %> OWL.AllDisjointProperties => Some(new OWLAllDisjointProperties(resource,triples))
      case _ %> _ %> OWL.AllDifferent => Some(new OWLAllDifferent(resource,triples))
      case _=> None
    } find (_ != None) match {
      case Some(group) => group
      case None => throw new OWLNoSuchEntityException
    }
  }
}      

/** A set of disjoint classes*/
class OWLAllDisjointClasses private[owl] (val resource : Resource, val triples : TripleSet) extends OWLDisjunctionGroup[OWLClass]  {
  protected def membersProp = OWL.members
  protected def clazz = OWL.AllDisjointClasses
  protected def make(resource : Resource, triples : TripleSet) = OWLClass(resource,triples)
  override def toString = "OWLAllDisjointClasses("+members.mkString(",")+")"
}

object OWLAllDisjointClasses {
  /** Create a new disjoint class group */
  def apply(clazzes : OWLClass*) = {
    val n = AnonymousNode
    val l = RDFList((clazzes map { _.resource }):_*)
    new OWLAllDisjointClasses(n, TripleSet(n %> RDF._type %> OWL.AllDisjointClasses, 
      n %> OWL.members %> l.node) ++ l.triples)
  }
  
  /** Make a disjoint class group at the given resource.
   * @throws OWLNoSuchEntityException If there is no disjoint group at this URI
   */
  def apply(resource : Resource, triples : TripleSet) = {
    if(triples has(Some(resource),Some(RDF._type),Some(OWL.AllDisjointClasses))) {
      new OWLAllDisjointClasses(resource,triples)
    } else {
      throw new OWLNoSuchEntityException()
    }
  }
}
 
/** A set of disjoint properties */
class OWLAllDisjointProperties private[owl] (val resource : Resource, val triples : TripleSet) extends OWLDisjunctionGroup[OWLProperty[OWLType]]  {
  protected def membersProp = OWL.members
  protected def clazz = OWL.AllDisjointProperties
  protected def make(resource : Resource, triples : TripleSet) = OWLProperty(resource,triples)
  override def toString = "OWLAllDisjointProperties("+members.mkString(",")+")"
}

object OWLAllDisjointProperties {
  /** Create a new disjoint property group */
  def apply(clazzes : OWLProperty[OWLType]*) = {
    val n = AnonymousNode
    val l = RDFList((clazzes map { _.resource }):_*)
    new OWLAllDisjointClasses(n, TripleSet(n %> RDF._type %> OWL.AllDisjointProperties, 
      n %> OWL.members %> l.node) ++ l.triples)
  }
  
  /** Make a disjoint group at the given resource.
   * @throws OWLNoSuchEntityException If there is no disjoint group at this URI
   */
  def apply(resource : Resource, triples : TripleSet) = {
    if(triples has(Some(resource),Some(RDF._type),Some(OWL.AllDisjointProperties))) {
      new OWLAllDisjointClasses(resource,triples)
    } else {
      throw new OWLNoSuchEntityException()
    }
  }
}

/** A set of all different individuals */
class OWLAllDifferent private[owl] (val resource : Resource, val triples : TripleSet) extends OWLDisjunctionGroup[OWLIndividual]  {
  protected def membersProp = OWL.distinctMembers
  protected def clazz = OWL.AllDifferent
  protected def make(resource : Resource, triples : TripleSet) = OWLIndividual(resource,triples)
  override def toString = "OWLAllDifferent("+members.mkString(",")+")"
}

object OWLAllDifferent {
  /** Create a new all different individual group */
  def apply(clazzes : OWLIndividual*) = {
    val n = AnonymousNode
    val l = RDFList((clazzes map { _.resource }):_*)
    new OWLAllDisjointClasses(n, TripleSet(n %> RDF._type %> OWL.AllDifferent, 
      n %> OWL.distinctMembers %> l.node) ++ l.triples)
  }
  
  
  /** Make a disjoint group at the given resource.
   * @throws OWLNoSuchEntityException If there is no disjoint group at this URI
   */
  def apply(resource : Resource, triples : TripleSet) = {
    if(triples has(Some(resource),Some(RDF._type),Some(OWL.AllDifferent))) {
      new OWLAllDisjointClasses(resource,triples)
    } else {
      throw new OWLNoSuchEntityException()
    }
  }
}
