# OWL Classes #

SSW attempts to provide an easy syntax for describing OWL Classes, while keeping to the principle of immutability.

As we recall a new class may be constructed as follows

```
scala> val c1 = OWLClass('C1)
c1: scalasemweb.owl.OWLClass = OWLClass(:C1)
```

If we define another class we can create some class relationships

```
scala> val c2 = OWLClass('C2)
c2: scalasemweb.owl.OWLClass = OWLClass(:C2)

scala> c1 subClassOf c2
res0: scalasemweb.owl.OWLClass = OWLClass(:C1)
```

Note that as classes are immutable `res0` is a new object, it is considered equal to `c1` but does not have the same set of triples, as demonstrated below:

```
scala> res0 == c1
res1: Boolean = true

scala> res0.triples == c1.triples
res2: Boolean = false

scala> c1.subClasses
res3: scala.collection.Set[scalasemweb.owl.OWLClass] = Set()

scala> res0.subClasses
res4: scala.collection.Set[scalasemweb.owl.OWLClass] = Set(OWLClass(:C2))
```

(As such res0 will replace C1 when added to an ontology)

In fact, SSW implements many functions on classes intended to imitate the functionality of Manchester OWL Syntax. In particular we have `and` and `or` to define class intersections and disjunctions

```
scala> c1 and c2
res9: scalasemweb.owl.OWLClassIntersection = OWLClassIntersection(OWLClass(:C1),OWLClass(:C2))

scala> c1 or c2
res10: scalasemweb.owl.OWLClassDisjunction = OWLClassDisjuction(OWLClass(:C1),OWLClass(:C2))
```
