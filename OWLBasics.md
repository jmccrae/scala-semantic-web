# OWL Basics #

SSW implements a simple method for using OWL, based on views as described here [TripleSetsViewsIO](TripleSetsViewsIO.md)


## Ontologies, Classes, Properties, Individuals ##

An ontology may be created by either of `OWLOntology`'s apply methods. So we can create a new ontology identified by a given resource as follows

```
scala> var onto = OWLOntology("""http://www.example.com/myonto"""uri)
onto: scalasemweb.owl.OWLOntology = Set()
```

Note that OWLOntology is also a set of OWLEntities so has normal Scala collection functions, such as find

We can create a class, property or individual similarly

```
scala> val example = NameSpace("example","""http://www.example.com/myonto#""")
example: scalasemweb.rdf.model.NameSpace = NameSpace(example,http://www.example.com/myonto#)

scala> val c1 = OWLClass(example&"C1")
c1: scalasemweb.owl.OWLClass = OWLClass(example:C1)
```

We may update the ontology as usual with +=

```
scala> onto += c1

scala> onto
res1: scalasemweb.owl.OWLOntology = Set(OWLClass(example:C1))
```

We can get the set of elements in an ontology by calling `classes`


```
scala> onto.classes
res2: scala.collection.Set[scalasemweb.owl.OWLClass] = Set(OWLClass(example:C1))
```

We may also create properties and individuals in a similar manner

```
scala> val dp = OWLDatatypeProperty(example&"dataprop")
dp: scalasemweb.owl.OWLDatatypeProperty = OWLDatatypeProperty(example:dataprop)

scala> val op = OWLObjectProperty(example&"objprop")
op: scalasemweb.owl.OWLObjectProperty = OWLObjectProperty(example:objprop)

scala> val indiv = OWLIndividual(example&"indiv")
indiv: scalasemweb.owl.OWLIndividual = OWLIndividual(example:indiv)
```

## Loading and Saving ontologies ##

The OWL extensions use the core RDF model to provide IO as such at any time you can call `triples` on the ontology object, to see all triples within the ontology

```
scala> onto += dp ; onto += op ; onto += indiv 


scala> onto.triples
res4: scalasemweb.rdf.model.TripleSet = Set(
example:objprop rdf:type owl:ObjectProperty, 
example:dataprop rdf:type owl:DatatypeProperty, 
example:C1 rdf:type owl:Class, 
<http://www.example.com/myonto> rdf:type owl:Ontology, 
example:indiv rdf:type owl:Thing)
```

Loading and Saving is then done by means of `TripleSet`s. e.g.,

```
scala> val newOnto = OWLOntology("""file:test#myonto"""uri,Turtle.parse(new FileReader("test.owl")))
newOnto: scalasemweb.owl.OWLOntology = Set(OWLObjectProperty(:p), OWLClass(:C))
```

And written in a similar manner

```
scala> Turtle.write(newOnto, new PrintStream("test.ttl"))
```