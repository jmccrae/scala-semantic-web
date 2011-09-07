package scalasemweb

import scalasemweb.rdf.model._;

package object owl {
  implicit def ontology2tripleset(ontology : OWLOntology) = ontology.triples
  
  implicit def rdf2annotation(value : Value) : OWLAnnotationValue = new OWLAnnotationValue {
    def value = value
  }
  
  implicit def annotation2rdf(annoVal : OWLAnnotationValue) : Value = annoVal.value
}
