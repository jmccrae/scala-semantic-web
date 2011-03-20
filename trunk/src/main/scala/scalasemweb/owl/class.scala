package scalasemweb.owl

import scala.collection._
import scalasemweb.owl.restrictions._
import scalasemweb.rdf.model._
import scalasemweb.rdf.collection._

/////////////////////////////////////////////////////////////////////////////////
// OWL classes

/** An OWL class */
class OWLClass private[owl] (val resource : Resource, val triples : TripleSet) extends OWLType {
  implicit def companion = OWLClass
  
  /** Create an OWL class that is identical this but has a set of subclass axioms */
  def subClassOf(clazzes : OWLClass*) = make(resource, triples ++
     (clazzes flatMap  
      (clazz => clazz.frame + (resource %> RDFS.subClassOf %> clazz.resource))
    ))
  
  /** Create an OWL class that is identical this but has a set of equivalent class axioms */
  def equivalentClass(clazzes : OWLClass*) = make(resource, triples ++ 
    (clazzes flatMap  
       (clazz => clazz.frame + (resource %> OWL.equivalentClass %> clazz.resource))
    ))
    
  /** Create an OWL class that is identical this but has a set of disjoint class axioms */
  def disjointWith(clazzes : OWLClass*) = make(resource, triples ++
    (clazzes flatMap  
      (clazz => clazz.frame + (resource %> OWL.disjointWith %> clazz.resource))
    ))
    
  /** Create an OWL class that is identical to this but with the following complement classes */
  def complementOf(clazzes : OWLClass*) = make(resource, triples ++
    (clazzes flatMap
      (clazz => clazz.frame + (resource %> OWL.complementOf %> clazz.resource))
    ))
    
  /** Create an OWL class that is identical this but has a set of disjoint union class axioms */
  def disjointUnionOf(clazzList : Iterable[OWLClass]) = {
    val list = RDFList.fromSeq(clazzList map (_.resource))
    make(resource, triples + (resource %> OWL.disjointUnionOf %> list.node) ++ list.frame)
  }
  
  /** Create an OWL class that is indentical to this but is deprecated */
  def deprecated(value : Boolean) = if(value) {
    make(resource, triples + (resource %> RDF._type %> OWL.DeprecatedClass))
  } else {
    make(resource, triples - (resource %> RDF._type %> OWL.DeprecatedClass))
  } 
    
  /** Get all subclasses of this class */
  def subClasses : Set[OWLClass] = triples get(Some(resource), Some(RDFS.subClassOf),None) map {
    case _ %> _ %> (res :Resource) => make(res,triples)
    case x => throw new OWLFormatException(x + " was stated as a sub class but is not a resource")
  }
  
  /** Get all equivalent classes of this class */
  def equivalentClasses : Set[OWLClass] = triples get(Some(resource), Some(OWL.equivalentClass),None) map {
    case _ %> _ %> (res :Resource) => make(res,triples)
    case x => throw new OWLFormatException(x + " was stated as an equivalent class but is not a resource")
  }
  
  /** Get all disjoint classes of this class */
  def disjointClasses : Set[OWLClass] = triples get(Some(resource), Some(OWL.disjointWith),None) map {
    case _ %> _ %> (res :Resource) => make(res,triples)
    case x => throw new OWLFormatException(x + " was stated as an equivalent class but is not a resource")
  }
  
  /** Get all disjoint unions with this class */
  def disjointUnions : Set[LinearSeq[OWLClass]] = triples get(Some(resource),Some(OWL.disjointUnionOf),None) map {
    case _ %> _ %> (res : Resource) => RDFList(res,triples) map {
      case res2 : Resource => make(res2,triples)
      case _ => throw new OWLFormatException(res + " was stated as part of a disjoint union of classes but is not a resource")
    }
    case x => throw new OWLFormatException(x + " was stated as a list of disjoint classes but is not a resource")
  }   
  
  /** Get all complementary classes to this class */
  def complements : Set[OWLClass] = triples get(Some(resource), Some(OWL.complementOf),None) map {
    case _ %> _ %> (res :Resource) => make(res,triples)
    case x => throw new OWLFormatException(x + " was stated as a complement class but is not a resource")
  }
  
  /** Get if this class is deprecated */
  def isDeprecated : Boolean = (triples get(Some(resource),Some(RDF._type),Some(OWL.DeprecatedClass))) != None
  
  /** Create a new class intersection between this class and the given class */
  def and(clazz : OWLClass) = OWLClassIntersection(this,clazz)
  
  /** Create a new class disjunction between this class and the given class */
  def or(clazz : OWLClass) = OWLClassDisjunction(this,clazz)
    
  def not(clazz : OWLClass) = {
    val n = AnonymousNode
    make(n,triples) complementOf this
  }
  
  private lazy val _frame = triples.get(Some(resource),None,None) ++
    (triples.get(Some(resource),Some(OWL.disjointUnionOf),None) flatMap {
      case triple : Triple => { triple.obj match {
        case res : Resource => RDFList(res,triples).frame 
        case res => throw new OWLFormatException(res + " was stated as part of a disjoint union of classes but is not a resource")
        }
      }
    })
    
  def frame = _frame
    
  override def equals(o : Any) = o match {
    case clazz : OWLClass => resource == clazz.resource
    case _ => false
  }
  
  override def hashCode = resource.hashCode + 1
  
  override def toString = "OWLClass("+resource+")"
}

object OWLClass extends OWLCompanion[OWLClass] {
  lazy val predefined = Set[Resource](OWL.Thing,OWL.Nothing)
  
  /** Create a new class with a given identifier */
  def apply(res : Resource) = new OWLClass(res, TripleSet(res %> RDF._type %> OWL._Class))
  /** Get the class at the given resource
  * @throws OWLNoSuchEntityException If there is no class at this URI
  */
  def apply(res : Resource, triples : TripleSet) = {
    if(triples.has(Some(res),Some(RDF._type),Some(OWL._Class)) ||
    triples.has(Some(res),Some(RDF._type),Some(RDFS._Class)) || predefined.contains(res)) {
      new OWLClass(res,triples)
    } else {
      triples.get(Some(res),Some(OWL.intersectionOf),None) headOption match {
        case Some(_ %> _ %> (listNode : Resource)) => new OWLClassIntersection(res,triples,RDFList(listNode,triples))
        case _ => {
          triples.get(Some(res),Some(OWL.disjointWith),None) headOption match {
            case Some(_ %> _ %> (listNode : Resource)) => new OWLClassDisjunction(res,triples,RDFList(listNode,triples))
            case _ => {
              triples.get(Some(res),Some(OWL.oneOf),None) headOption match {
                case Some(_ %> _ %> (listNode : Resource)) => new OWLOneOf(res,triples,RDFList(listNode,triples))
                case _ => OWLRestriction(res,triples)
              }
            }
          }
        }
      }
    }
  }
  def unapply(entity : OWLEntity) : Option[Resource] = {
    if(entity.triples has(Some(entity.resource),Some(RDF._type),Some(OWL._Class))) {
      Some(entity.resource)
    } else {
      None
    }
  }
}

/////////////////////////////////////////////////////////////////////////////////
// OWL complex classes

/** Class defined by the intersection of two other classes */
class OWLClassIntersection private[owl] (resource :Resource, triples : TripleSet, val list : RDFList) 
    extends OWLClass(resource,triples++list) {
  override def frame = list.frame

  /** Create a new class intersection including the given class */
  override def and(clazz : OWLClass) = new OWLClassIntersection(resource,triples,list :+ clazz.resource)
  
  /** The classes that constitute this intersection */
  def intersections : Set[OWLClass] = (list map {
    case res : Resource => make(res,triples)
    case lit => throw new OWLFormatException(lit + " was stated as part of an OWL class intersection but is not a resource")
  }).toSet
  
  override def toString = "OWLClassIntersection("+ intersections.mkString(",") + ")"
}

object OWLClassIntersection {
  /** Create a new class intersection of the given classes */
  def apply(clazz : OWLClass*) = {
    val n = AnonymousNode
    val l = RDFList((clazz map (_.resource)):_*)
    new OWLClassIntersection(n,mergeClasses(clazz) + (n %> OWL.intersectionOf %> l.node),l)
  }
  
  /** Get the owl class intersection at this resource.
   * @throws OWLNoSuchEntityException If there is no class intersection at this URI
   */
  def apply(resource : Resource, triples : TripleSet) = {
    triples.get(Some(resource),Some(OWL.intersectionOf),None) headOption match {
      case Some(_ %> _ %> (r : Resource)) => new OWLClassIntersection(resource,triples,RDFList(r,triples))
      case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as part of an OWL class intersection but is not a resource")
      case None => throw new OWLNoSuchEntityException
    }
  }
  
  private def mergeClasses(clazz : Iterable[OWLClass]) : TripleSet = {
    if(clazz.isEmpty) {
      TripleSet()
    } else {
      var ts = clazz.head.triples
      for(c <- clazz.tail) {
        ts ++= c.triples
      }
      ts
    }
  }
}

/** A disjunction of classes */
class OWLClassDisjunction private[owl] (resource : Resource, triples : TripleSet, val list : RDFList) 
    extends OWLClass(resource,triples++list) {
  override def frame = list.frame
  
  /** Create a new disjunction that extends the disjunction with the given class */
  override def or(clazz : OWLClass) = new OWLClassDisjunction(resource,triples,list :+ clazz.resource)
  
  /** The set of classes that compose this disjunction */
  def disjunctions : Set[OWLClass] = {
    list map {
      case res : Resource => make(res,triples)
      case lit => throw new OWLFormatException(lit + " was stated as part of an OWL class disjunction but is not a resource")
    }
  }.toSet
  
  override def toString = "OWLClassDisjuction("+disjunctions.mkString(",") + ")"
}

object OWLClassDisjunction {
  /** Create a disjunction of the given classes */
  def apply(clazz : OWLClass*) = {
    val n = AnonymousNode
    val l = RDFList((clazz map (_.resource)):_*)
    new OWLClassDisjunction(n,mergeClasses(clazz) + (n %> OWL.intersectionOf %> l.node),l)
  }
  
  /** Get the owl class disjunction at this resource.
   * @throws OWLNoSuchEntityException If there is no class intersection at this URI
   */
  def apply(resource : Resource, triples : TripleSet) = {
    triples.get(Some(resource),Some(OWL.intersectionOf),None) headOption match {
      case Some(_ %> _ %> (l : Resource)) => new OWLClassDisjunction(resource,triples,RDFList(l,triples))
      case Some(_ %> _ %> r) => throw new OWLFormatException(r + " was stated as part of an OWL class disjunction but is not a resource")
      case None => throw new OWLNoSuchEntityException
    }
  }
  
  private def mergeClasses(clazz : Iterable[OWLClass]) : TripleSet = {
    if(clazz.isEmpty) {
      TripleSet()
    } else {
      var ts = clazz.head.triples
      for(c <- clazz.tail) {
        ts ++= c.triples
      }
      ts
    }
  }
}

/** A class equal to a set of values */
class OWLOneOf private[owl] (resource : Resource, triples : TripleSet, val list : RDFList)
    extends OWLClass(resource,triples++list) {
  override def frame = list.frame
  
  def elements : List[OWLIndividual] = (list map { x:Value => x match {
    case x : Resource => new OWLIndividual(x,triples)
    case lit => throw new OWLFormatException(lit + " indicated as an OWL one of but not a resource")
  }}).toList
}
 
object OWLOneOf {
  /** Create a new set of values */
  def apply(indivs : OWLIndividual*) = {
    val n = AnonymousNode
    val l = RDFList((indivs map (_.resource)):_*)
    new OWLOneOf(n,mergeIndivs(indivs)++l.triples+(n %> OWL.oneOf %> l.node),l)
  }
  
  private def mergeIndivs(clazz : Iterable[OWLIndividual]) : TripleSet = {
    if(clazz.isEmpty) {
      TripleSet()
    } else {
      var ts = clazz.head.triples
      for(c <- clazz.tail) {
        ts ++= c.triples
      }
      ts
    }
  }
  
  /** Get the set of values at this resource.
   * @throws OWLNoSuchEntityException If there is no set of values at this URI
   */
  def apply(resource : Resource, triples : TripleSet) = {
    triples.get(Some(resource),Some(OWL.oneOf),None) headOption match {
      case Some(_ %> _ %> (x : Resource)) => new OWLOneOf(resource,triples,RDFList(x,triples))
      case _ => throw new OWLNoSuchEntityException
    }
  }
  
  def unapplySeq(entity : OWLEntity) : Option[Seq[OWLIndividual]] = {
    entity match {
      case oneOf : OWLOneOf => Some(oneOf.elements)
      case _ => None
    }
  }
}

/** Pattern matching for intersections */
object and {
  def unapply(entity : OWLEntity) : Option[Tuple2[OWLClass,OWLClass]] = entity match {
    case intersection : OWLClassIntersection => if(intersection.list.tail.isEmpty) {
      None
    } else if(intersection.list.tail.tail.isEmpty) {
      Some((new OWLClass(intersection.list.head.asInstanceOf[Resource],intersection.triples),
        new OWLClass(intersection.list.tail.head.asInstanceOf[Resource],intersection.triples)))
    } else {
      Some((new OWLClassIntersection(intersection.resource,intersection.triples,intersection.list.tail),
        new OWLClass(intersection.list.head.asInstanceOf[Resource],intersection.triples)))
    }
  }
}
      
/** Pattern matching for disjunctions */
object or {
  def unapply(entity : OWLEntity) : Option[Tuple2[OWLClass,OWLClass]] = entity match {
    case disjunction : OWLClassDisjunction => if(disjunction.list.tail.isEmpty) {
      None
    } else if(disjunction.list.tail.tail.isEmpty) {
      Some((new OWLClass(disjunction.list.head.asInstanceOf[Resource],disjunction.triples),
        new OWLClass(disjunction.list.tail.head.asInstanceOf[Resource],disjunction.triples)))
    } else {
      Some((new OWLClassDisjunction(disjunction.resource,disjunction.triples,disjunction.list.tail),
        new OWLClass(disjunction.list.head.asInstanceOf[Resource],disjunction.triples)))
    }
  }
}

// Note not doesn't work in pattern matching because it is not a single value

