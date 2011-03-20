package scalasemweb.owl

import scala.collection._
import scalasemweb.rdf.model._
import scalasemweb.rdf.collection._

/////////////////////////////////////////////////////////////////////////////////
// OWL ontology

/** An instance of an OWL ontology. The SSW OWL package is designed as a set of views on
 * triple sets, the OWL ontology object views all triples in the ontology. 
 */
class OWLOntology private[owl](val resource : Resource, val triples : TripleSet) extends View with Set[OWLEntity] 
with SetLike[OWLEntity,OWLOntology] {
  /** Create a copy of this ontology with the entity added */
  def +(entity : OWLEntity) = new OWLOntology(resource,triples ++ entity.frame)
  /** Create a copy of this ontology with the entity removed */
  def -(entity : OWLEntity) = new OWLOntology(resource,triples -- entity.frame)
  /** Does this contain the given entity */
  def contains(entity : OWLEntity) = !(triples get(Some(entity.resource),Some(RDF._type),None) isEmpty)
  
  private lazy val entSet : Set[OWLEntity] = ((triples get(None,None,None) flatMap {
    case s %> p %> (o : Resource) => Set(s,p,o)
    case s %> p %> o => Set(s,p)
  }) -- OWLClass.predefined -- OWLObjectProperty.predefined -- OWLDatatypeProperty.predefined -- 
    OWLAnnotationProperty.predefined) flatMap {
    resource => clazz(resource) ++ property(resource) ++ indiv(resource)
  } 
  
  private def clazz(resource : Resource) : Option[OWLEntity] = try {
    Some(OWLClass(resource,triples))
  } catch {
    case x : OWLNoSuchEntityException => None
  }
  
  private def property(resource : Resource) : Option[OWLEntity] = try {
    Some(OWLProperty(resource,triples))
  } catch {
    case x : OWLNoSuchEntityException => None
  }
  
  private def indiv(resource : Resource) : Option[OWLEntity] = try {
    Some(OWLIndividual(resource,triples))
  } catch {
    case x : OWLNoSuchEntityException => None
  }
  
  /** Get an iterator over the set of elements */
  def iterator = entSet.iterator
  /** Get a new empty ontology */
  override def empty = new OWLOntology(resource,TripleSet())
  /** The frame of the ontology is always the set of triples */
  def frame = triples
  /** Get all entities in the ontology */
  def entities = entSet
  /** Get all classes in the ontology */
  def classes = entSet flatMap {
    case x : OWLClass => Some(x)
    case _ => None
  }
  /** Get all properties in the ontology */
  def properties : Set[OWLProperty[OWLType]] = entSet flatMap {
    case x : OWLProperty[OWLType] => Some(x)
    case _ => None
  }
  /** Get all object properties in the ontology */
  def objectProperties : Set[OWLObjectProperty] = entSet flatMap {
    case x : OWLObjectProperty => Some(x)
    case _ => None
  }
  /** Get all datatype properties in the ontology */
  def datatypeProperties : Set[OWLDatatypeProperty] = entSet flatMap {
    case x : OWLDatatypeProperty => Some(x)
    case _ => None
  }
  /** Get all annotation properties in the ontology. This is only the properties defined in this ontology not all used */
  def annotationProperties : Set[OWLAnnotationProperty] = entSet flatMap {
    case x : OWLAnnotationProperty => Some(x)
    case _ => None
  }
  /** Get all individuals in the ontology */
  def individuals : Set[OWLIndividual] = entSet flatMap {
    case x : OWLIndividual => Some(x)
    case _ => None
  }
  
  /** Get all disjoint group statements in the ontology */
  def disjointGroups : Set[OWLDisjunctionGroup[OWLEntity]] = {
    disjointClasses.asInstanceOf[Set[OWLDisjunctionGroup[OWLEntity]]] ++ 
    disjointProperties.asInstanceOf[Set[OWLDisjunctionGroup[OWLEntity]]] ++ 
    differentIndividuals.asInstanceOf[Set[OWLDisjunctionGroup[OWLEntity]]]
  }
  
  /** Get all disjoint class groups in the ontology */
  def disjointClasses = triples.get(None,Some(RDF._type),Some(OWL.AllDisjointClasses)) map {
    case x %> _ %> _ => OWLAllDisjointClasses(x,triples)
  }
  
  /** Get all disjoint class groups in the ontology */
  def disjointProperties = triples.get(None,Some(RDF._type),Some(OWL.AllDisjointProperties)) map {
    case x %> _ %> _ => OWLAllDisjointProperties(x,triples)
  }
  
  /** Get all disjoint class groups in the ontology */
  def differentIndividuals = triples.get(None,Some(RDF._type),Some(OWL.AllDifferent)) map {
    case x %> _ %> _ => OWLAllDifferent(x,triples)
  }
  
  /** Create a new version of this ontology with the given annotation */
  def annotation(annos : Tuple2[OWLAnnotationProperty,OWLAnnotationValue]*) = {
    new OWLOntology(resource,annos.foldLeft(triples)((x,y) => x + (resource %> y._1.resource %> y._2.value)))
  }
  
  /** Get all annotations attached to this element */
  def annotations = {
    triples.get(Some(resource),None,None) flatMap {
      case _ %> p %> o => try {
        val ap = OWLAnnotationProperty(p,triples)
        val av = o match {
          case av : Literal => new OWLAnnotationValue {  def value = av }
          case av : Resource => {
            try {
              OWLIndividual(av,triples)
            } catch {
              case x : OWLNoSuchEntityException => new OWLAnnotationValue { def value = av }
            }
          }
        }
        Some(ap -> av)
      } catch {
        case x : OWLNoSuchEntityException => None
      }
    }
  }
}

object OWLOntology {
  /** Create a new ontology, identified by a resource */
  def apply(resource : Resource) = new OWLOntology(resource,TripleSet(resource %> RDF._type %> OWL.Ontology))
  /** Create a new ontology from a triple set */
  def apply(resource : Resource, triples : TripleSet) = new OWLOntology(resource,triples)
}

