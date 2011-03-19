package scalasemweb.owl

import scala.collection._
import scalasemweb.rdf.model._
import scalasemweb.rdf.collection._

/////////////////////////////////////////////////////////////////////////////////
// OWL ontology

/** An instance of an OWL ontology. The SSW OWL package is designed as a set of views on
 * triple sets, the OWL ontology object views all triples in the ontology. It is also
 * accessible as an immutable set so changes can be made
 * @param triples The underlying set of triples
 */
class OWLOntology (val triples : TripleSet) extends View with Set[OWLEntity] 
with SetLike[OWLEntity,OWLOntology] {
  /** Create a copy of this ontology with the entity added */
  def +(entity : OWLEntity) = new OWLOntology(triples ++ entity.frame)
  /** Create a copy of this ontology with the entity removed */
  def -(entity : OWLEntity) = new OWLOntology(triples -- entity.frame)
  /** Does this contain the given entity */
  def contains(entity : OWLEntity) = !(triples get(Some(entity.resource),Some(RDF._type),None) isEmpty)
  private lazy val entSet : Set[OWLEntity] = triples get(None,Some(RDF._type),None) map {
    case res %> _ %> typ => typ match {
      case OWL._Class => new OWLClass(res,triples)
      case RDFS.Datatype => new OWLDatatype(res,triples)
      case OWL.ObjectProperty => new OWLObjectProperty(res.asInstanceOf[NamedNode],triples)
      case OWL.DatatypeProperty => new OWLDatatypeProperty(res.asInstanceOf[NamedNode],triples)
      case OWL.AnnotationProperty => new OWLAnnotationProperty(res.asInstanceOf[NamedNode],triples)
      case _ => new OWLIndividual(res,triples)
    }
  }
  /** Get an iterator over the set of elements */
  def iterator = entSet.iterator
  /** Get a new empty ontology */
  override def empty = new OWLOntology(TripleSet())
  /** The frame of the ontology is always the set of triples */
  def frame = triples
  
  def entities = classes ++ properties ++ individuals
 
 
  def classes = (triples.get(None,Some(RDF._type),Some(OWL._Class)) map {
    case res %> _ %> _ => Some(new OWLClass(res,triples))
    case _ => None
  }) ++ (triples.get(None,Some(RDF._type),Some(RDFS._Class)) map {
    case res %> _ %> _ => Some(new OWLClass(res,triples))
    case _ => None
  })
  
  def properties : Set[OWLProperty[OWLType]] = {
    objectProperties.asInstanceOf[Set[OWLProperty[OWLType]]] ++ 
    datatypeProperties.asInstanceOf[Set[OWLProperty[OWLType]]] ++ 
    annotationProperties.asInstanceOf[Set[OWLProperty[OWLType]]]
  }
  
  def objectProperties : Set[OWLObjectProperty] = {
      triples.get(None,Some(RDF._type),Some(OWL.ObjectProperty)) flatMap {
        case res %> _ %> _ => Some(new OWLObjectProperty(res.asInstanceOf[NamedNode],triples))
        case _ => None
      }
   }
 
  def datatypeProperties : Set[OWLDatatypeProperty] = {
    triples.get(None,Some(RDF._type),Some(OWL.DatatypeProperty)) flatMap {
      case res %> _ %> _ => Some(new OWLDatatypeProperty(res.asInstanceOf[NamedNode],triples))
      case _ => None
    }
  }
 
  def annotationProperties : Set[OWLAnnotationProperty] = {
   triples.get(None,Some(RDF._type),Some(OWL.AnnotationProperty)) flatMap {
      case res %> _ %> _ => Some(new OWLAnnotationProperty(res.asInstanceOf[NamedNode],triples))
      case _ => None
    }
  }
  
  def individuals : Set[OWLIndividual] = {
    triples.get(None,Some(RDF._type),None) flatMap {
      case res %> _ %> typ => if(typ == OWL._Class || typ == RDFS._Class || typ == OWL.ObjectProperty ||
        typ == OWL.DatatypeProperty || typ == OWL.AnnotationProperty || typ == RDF.Property) {
        None
      } else {
        Some(new OWLIndividual(res,triples))
      }
    }
  }
}

object OWLOntology {
  /** Create a new ontology, identified by a resource */
  def apply(resource : Resource) = new OWLOntology(TripleSet(resource %> RDF._type %> OWL.Ontology))
  /** Create a new ontology from a triple set */
  def apply(triples : TripleSet) = new OWLOntology(triples)
}

