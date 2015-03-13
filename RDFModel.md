# Scala Semantic Web RDF Model #

This briefly describes how to work with the Scala Semantic Web library's RDF modeling.

## Working with RDF values ##

The Scala Semantic Web library's most basic function is as a RDF model. The structure of the RDF model is follows

  * Value: _Any RDF value_
    * Resource: _Any RDF resource_
      * NamedNode: _A RDF value with a URI_
        * URIRef: _A RDF value identified by an unqualified URI_
        * QName: _A RDF value identified by a prefix name space and a suffix_
      * BlankNode: _A RDF value without a URI_
    * Literal: _A RDF data value_
      * SimpleLiteral: _An untyped value without langauge_
      * LangLiteral: _A value with a language tag_
      * TypedLiteral: _A typed data value_

Each of these data types can be created in the usual way, e.g.,

```
 val x = URIRef(URI.create("http://dbpedia.org/resource/Scala"))
```

These may be created in an easier manner using implicit methods

```
scala> import scalasemweb.rdf.model._                                                                                                    
import scalasemweb.rdf.model._

scala> "http://dbpedia.org/property/designer"uri
res0: scalasemweb.rdf.model.URIRef = <http://dbpedia.org/property/designer>

scala> RDF&"type"
res1: scalasemweb.rdf.model.QName = rdf:type

scala> "string"@@"en"
res2: scalasemweb.rdf.model.LangLiteral = "string"@en

scala> "string"^^XSD.string
res3: scalasemweb.rdf.model.TypedLiteral = "string"^^xsd:string
```

In addition each triple can be created as an instance of type triple

```
  val trip = Triple(URIRef("http://dbpedia.org/page/Scala_%28programming_language%29"),
   URIRef("http://dbpedia.org/property/designer"),
   SimpleLiteral("Martin Odersky"))
```

However triples may be constructed in an easier manner using the `%>` operator

```
scala> ("http://dbpedia.org/page/Scala_%28programming_language%29"uri) %> ("http://dbpedia.org/property/designer"uri) %> "Martin Odersky"

res4: scalasemweb.rdf.model.Statement = <http://dbpedia.org/page/Scala_%28programming_language%29> <http://dbpedia.org/property/designer> "Martin Odersky"
```

Case matching on statements is also supported

```
scala> res4 match { case _ %> _ %> obj => obj }
res5: scalasemweb.rdf.model.Value = "Martin Odersky"
```

## Name spaces and built-in values ##

SSW supports name spaces through the `NameSpace` object, which can be created as follows

```
val dbpprop = NameSpace("dbpprop","http://dbpedia.org/property/")
```

As noted above a URI in the name space can be created with the `&` operator

```
scala> dbpprop&"designer"
res6: scalasemweb.rdf.model.QName = dbpprop:designer
```

In addition the namespaces for RDF, RDFS and XSD come predefined with the library

```
scala> RDFS.label
res7: scalasemweb.rdf.model.QName = rdfs:label
```

It should be noted some names have an underscore underneath them to avoid clashes with Scala keywords and generally used library classes

```
scala> RDF._type
res8: scalasemweb.rdf.model.QName = rdf:type
```

It is easy to define a name space, that has the same features

```
object MyNameSpace extends NameSpace("my","http://www.mynamespace.com/") {
  val myProp = this&"myProp"
}
```

In addition the RDF has a special variable `base`, which allows name spaces to be worked without like defining an empty prefix in Turtle

```
scala> RDF.base = "http://dbpedia.org/resource/"

scala> 'Scala %> (dbpprop&"designer") %> "Martin Odersky"
res9: scalasemweb.rdf.model.Statement = :Scala dbpprop:designer "Martin Odersky"
```

This feature should be used with caution as it sets a global variable

## SSW vs Turtle ##

These features combine to give SSW a Turtle inspired syntax, here we will show a side-by-side comparison

### Turtle ###
```
@prefix : <http://dbpedia.org/resource/> .
@prefix dbpprop: <http://dbpedia.org/dbpprop/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema> .

:Martin_Odersky dbpprop:birthDate "1958-09-03"^^xsd:date ;
   dbpprop:knownFor "Scala"@en ;
   dbpprop:nationality :Germany .
```

### SSW ###
```
RDF.base = "http://dbpedia.org/resource/" 
val dbpprop = NameSpace("dbpprop","http://dbpedia.org/property/") 

'Martin_Odersky %>* ( (dbpprop&"birthDate") %> ("1958-09-03"^^XSD.date),
   (dbpprop&"knownFor") %> ("Scala"@@"en"),
   (dbpprop&"nationality") %> 'Germany )
```