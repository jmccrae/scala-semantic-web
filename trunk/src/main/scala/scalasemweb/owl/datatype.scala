package scalasemweb.owl

import scalasemweb.rdf.collection._;
import scala.collection._;
import scalasemweb.rdf.model._;

/////////////////////////////////////////////////////////////////////////////////
// OWL datatypes

/** An OWL datatype. That is a value used to identify a range of literals */
class OWLDatatype private[owl] (val resource : Resource, val triples : TripleSet) extends OWLType {
  implicit def companion = OWLDatatype
  
  def frame = triples get(Some(resource),None,None)
  
  override def equals(o : Any) = o match {
      case clazz : OWLClass => resource == clazz.resource
      case _ => false
    }
    
  override def hashCode = resource.hashCode + 2
    
  override def toString = "OWLClass("+resource+")"
}

object OWLDatatype extends OWLCompanion[OWLDatatype] {
  lazy val predefined = Set[Resource](XSD._boolean,
    XSD._byte,
    XSD._double,
    XSD._float,
    XSD._long,
    XSD._short,
    XSD.duration,
    XSD.dateTime,
    XSD.time,
    XSD.date,
    XSD.gYearMonth,
    XSD.gYear,
    XSD.gMonthDay,
    XSD.gDay,
    XSD.gMonth,
    XSD.base64Binary,
    XSD.hexBinary,
    XSD.anyURI,
    XSD._QName,
    XSD.NOTATION,
    XSD.string,
    XSD.normalizedString,
    XSD.token,
    XSD.language,
    XSD.Name,
    XSD.NCName,
    XSD.ID,
    XSD.IDREF,
    XSD.IDREFS,
    XSD.ENTITY,
    XSD.ENTITIES,
    XSD.decimal,
    XSD.integer,
    XSD.nonPositiveInteger,
    XSD.negativeInteger,
    XSD._int,
    XSD.nonNegativeInteger,
    XSD.unsignedLong,
    //XSD.positiveLong,
    XSD.unsignedInt,
    XSD.unsignedByte,
    XSD.unsignedShort)
  
  def apply(resource : Resource) = new OWLDatatype(resource,TripleSet(resource %> RDF._type %> RDFS.Datatype))
  def apply(resource : Resource,triples : TripleSet) = {
    if(triples.has(Some(resource),Some(RDF._type),Some(RDFS.Datatype)) ||
    predefined.contains(resource)) {
      new OWLDatatype(resource,triples)
    } else {
      throw new OWLNoSuchEntityException
    }
  }
  def unapply(entity : OWLEntity) : Option[Resource] = {
    if(entity.triples has(Some(entity.resource),Some(RDF._type),Some(RDFS.Datatype))) {
      Some(entity.resource)
    } else {
      None
    }
  }
}

