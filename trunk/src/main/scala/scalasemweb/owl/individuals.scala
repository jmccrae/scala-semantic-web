package scalasemweb.owl

import scalasemweb.rdf.collection._;
import scala.collection._;
import scalasemweb.rdf.model._;


/////////////////////////////////////////////////////////////////////////////////
// OWL individuals

/** An OWL individual. A single instance of an object in the ontology */
class OWLIndividual private[owl] (val resource : Resource, val triples : TripleSet) extends OWLEntity with OWLAnnotationValue {
  implicit def companion = OWLIndividual
      
  /** Get a copy of this individual with a type axiom */
  def _type(clazzes : OWLClass*) = make(resource,triples ++ (clazzes map {
    clazz => resource %> RDF._type %> clazz.resource
  }))
  
  /** Get a copy of this individual with a same as axiom */
  def sameAs(indivs : OWLIndividual*) = make(resource,triples ++ (indivs map {
    indiv => resource %> OWL.sameAs %> indiv.resource
  }))
  
  /** Get a copy of this individual with a different from axiom */
  def differentFrom(indivs : OWLIndividual*) = make(resource,triples ++ (indivs map {
    indiv => resource %> OWL.differentFrom %> indiv.resource
  }))
  
  /** Get all types for this individual */
  def _types = triples get(Some(resource),Some(RDF._type),None) map {
    case _ %> _ %> (res : Resource) =>  OWLClass(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " was stated as a type of an individual but is not a resource")
  }
  
  /** Get all individuals that are equivalent to this individual */ 
  def sameIndividuals = triples get(Some(resource),Some(OWL.sameAs),None) map {
    case _ %> _ %> (res : Resource) =>  make(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " was stated as the same as an individual but is not a resource")
  }
   
  /** Get all individuals that are different from this individual */
  def differentIndividuals = triples get(Some(resource),Some(OWL.differentFrom),None) map {
    case _ %> _ %> (res : Resource) =>  new OWLClass(res,triples)
    case stat => throw new OWLFormatException(stat.obj + " was stated as different from an individual but is not a resource")
  }
  
  /** Get a copy of this individual with a given fact */
  def fact(prop : OWLDatatypeProperty, value : Value) =  make(resource,triples + (resource %> prop.resource %> value))
  
  /** Get a copy of this individual with a given fact */
  def fact(prop : OWLObjectProperty, value : OWLIndividual) =  make(resource,triples + (resource %> prop.resource %> value.resource))
  
  /** Get a copy of this individual with the given negative fact */
  def fact_not(prop : OWLDatatypeProperty, value : Value) = {
    val n = AnonymousNode
    make(resource,triples +
      (n %> RDF._type %> OWL.NegativePropertyAssertion) +
      (n %> OWL.sourceIndividual %> resource) +
      (n %> OWL.assertionProperty %> prop.resource) +
      (n %> OWL.targetValue %> value))
  }
  
  /** Get a copy of this individual with the given negative fact */
  def fact_not(prop : OWLObjectProperty, value : OWLIndividual) = {
    val n = AnonymousNode
    make(resource,triples +
      (n %> RDF._type %> OWL.NegativePropertyAssertion) +
      (n %> OWL.sourceIndividual %> resource) +
      (n %> OWL.assertionProperty %> prop.resource) +
      (n %> OWL.targetIndividual %> value.resource))
  }
  
  /** Get all (positive) facts about this individual */
  def facts : Set[Tuple2[OWLProperty[_],Any]] = {
    def foo(triple : Triple) : Option[Tuple2[OWLProperty[_],Any]] = triple match {
      case _ %> p %> (o : Resource) => try {
        val prop = OWLProperty(p,triples)
        prop match {
          case prop : OWLObjectProperty => Some(prop -> OWLIndividual(o,triples))
          case prop : OWLDatatypeProperty => Some(prop -> o)
          case _ => None
        }
      } catch {
        case x : OWLNoSuchEntityException => None
      }
      case _ %> p %> (o : Literal) => try {
        Some(OWLDatatypeProperty(p,triples) -> o)
      } catch {
        case x : OWLNoSuchEntityException => None
      }
    }
    triples.get(Some(resource),None,None) flatMap (foo _) 
  }
  
  /** Get all negative facts about this individual */
  def negativeFacts : Set[Tuple2[OWLProperty[_],Any]] = {
    def foo(triple : Triple) : Tuple2[OWLProperty[_],Any] = triple match {
      case res %> _ %> _ => {
        triples.get(Some(res),Some(OWL.targetIndividual),None) headOption match {
          case Some(_ %> _ %> (o : Resource)) => {
            val indiv = OWLIndividual(o,triples)
            triples.get(Some(res),Some(OWL.assertionProperty),None) headOption match {
              case Some(_ %> _ %> (n : NamedNode)) => (OWLObjectProperty(n,triples),indiv)
              case _ => throw new OWLFormatException("Negative Property Assertion without property")
            }
          }
          case _ => {
            triples.get(Some(res),Some(OWL.targetValue),None) headOption match {
              case Some(_ %> _ %> v) => {
                triples.get(Some(res),Some(OWL.assertionProperty),None) headOption match {
                  case Some(_ %> _ %> (n : NamedNode)) => (OWLDatatypeProperty(n,triples),v)
                  case _ => throw new OWLFormatException("Negative Property Assertion without property")
                }
              }
              case _ => throw new OWLFormatException("Negative Property Assertion without target")
            }
          }
        }
      }
    }
    triples.get(None,Some(OWL.sourceIndividual),Some(resource)) map (foo _)
  }
      
  
  def frame = triples get(Some(resource),None,None)
  
  override def equals(o : Any) = o match {
    case prop : OWLAnnotationProperty => resource == prop.resource
    case _ => false
  }
  
  override def hashCode = resource.hashCode + 6
    
  override def toString = "OWLIndividual("+resource+")"
  
  def value = resource
}

object OWLIndividual extends OWLCompanion[OWLIndividual] {
  def apply(resource : Resource) = new OWLIndividual(resource,TripleSet(resource %> RDF._type %> OWL.Thing))
  def apply(resource : Resource,triples : TripleSet) : OWLIndividual = {
    triples.get(Some(resource),Some(RDF._type),None).view.map {
      case _ %> _ %> OWL.Thing => Some(new OWLIndividual(resource,triples))
      case _ %> _ %> OWL.NamedIndividual => Some(new OWLIndividual(resource,triples))
      case _ %> _ %> (x : Resource) => try {
        OWLClass(x,triples)
        Some(new OWLIndividual(resource,triples))
      } catch {
        case x : OWLNoSuchEntityException => None
      }
      case _ => None
    } find (_ != None) match {
      case Some(indiv) => indiv.get
      case None => throw new OWLNoSuchEntityException("The individual for " + resource + " was either not found, or "+
       "was not an instance of owl:Thing or an OWLClass")
    }
  } 
   
  def unapply(entity : OWLEntity) : Option[Resource] = {
    entity.triples get(Some(entity.resource),Some(RDF._type),None) find {
      case _ %> _ %> OWL._Class => false
      case _ %> _ %> OWL.ObjectProperty => false
      case _ %> _ %> OWL.DatatypeProperty => false
      case _ %> _ %> OWL.AnnotationProperty => false
      case _ %> _ %> RDFS.Datatype => false
      case _ => true
    } match {
      case Some(_) => Some(entity.resource)
      case None => None
    }
  }
}   

