# Triple Sets, Views and IO #

## Triple Sets ##

The standard interface used by SSW for repositories and triple stores is `TripleSet`, which extends Scala's `Set` class so that it can be used much like a normal part of the Scala collection API e.g.,

```
scala> val tripleSet = TripleSet('subject %> 'property %> "object")
tripleSet: scalasemweb.rdf.model.TripleSet = Set(:subject :property "object")

scala> val tripleSet = TripleSet('subject %> 'property %> "object") map ( _.pred )
tripleSet: scala.collection.Set[scalasemweb.rdf.model.NamedNode] = Set(:property)
```

`TripleSet` is immutable like the standard Scala `Set`.

## Views ##

SSW implements `View` (not to be confused with views in the Scala API), which allow for a particular subset of triples to be brought into focus. Each view has two main functions `frame` indicating the set of triples of interest to the particular view and `triples` showing all triples visible to a view.

An example of views is the `RDFList` class that implements RDF lists. These may then be constructed in by means of their apply functions:

```
scala> val list = RDFList('e1,'e2,'e3)
list: scalasemweb.rdf.collection.RDFList = RDFListNode(:e1, :e2, :e3)
```

Note that RDFList also implements `LinearSeq` so normal Scala collection functions are available.

We can see the set of triples that compose this list by calling `frame`

```
scala> list.frame
res0: scalasemweb.rdf.model.TripleSet = Set(_:5041428934518591551 rdf:rest _:2891265432262758614,
 _:2964724138256440626 rdf:first :e1,
 _:2891265432262758614 rdf:rest rdf:nil,
 _:2964724138256440626 rdf:rest _:5041428934518591551,
 _:2891265432262758614 rdf:first :e3,
 _:5041428934518591551 rdf:first :e2)
```

We may also extract a list by using apply functions

```
scala> RDFList(list.node, list.frame + ('e1 %> RDF._type %> RDF.Property))
res1: scalasemweb.rdf.collection.RDFList = RDFListNode(:e1, :e2, :e3)
```

(Note here we add an irrelevant triple to the frame of the RDFList)

## IO ##

There are currently parser for Turtle and RDF/XML implemented

For example to load a Turtle file the following code is used:

```
scala> Turtle.parse(new java.io.FileReader("test.ttl"))
res3: scalasemweb.rdf.model.TripleSet = Set(:C rdf:type owl:Class,
...
```

Similarly we may write the file as follows

```
scala> Turtle.write(res3, new PrintStream("out.ttl"))
```