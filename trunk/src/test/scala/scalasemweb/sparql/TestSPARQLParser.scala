package scalasemweb.sparql

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class TestSPARQLParser extends FlatSpec with ShouldMatchers {
   val testQuery1 = """
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?name WHERE {
            <http://dbpedia.org/resource/Tallinn> rdfs:label ?name . FILTER (langMatches(lang(?name), "en"))
        }"""
        
   val testQuery2 = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
   SELECT * WHERE { 
   {?city rdfs:label 'Nathavas'@en.} UNION 
   { ?alias <http://dbpedia.org/property/redirect> ?city;  rdfs:label 'Nathavas'@en. } UNION 
   { ?alias <http://dbpedia.org/property/disambiguates> ?city;  rdfs:label 'Nathavas'@en. } 
   OPTIONAL { ?city <http://dbpedia.org/ontology/abstract> ?abstract} 
   OPTIONAL { ?city geo:lat ?latitude; geo:long ?longitude}OPTIONAL { ?city foaf:depiction ?image } 
   OPTIONAL { ?city rdfs:label ?name } 
   OPTIONAL { ?city foaf:homepage ?home } 
   OPTIONAL { ?city <http://dbpedia.org/ontology/populationTotal> ?population } 
   OPTIONAL { ?city <http://dbpedia.org/ontology/thumbnail> ?thumbnail } FILTER (langMatches( lang(?abstract), 'en'))
   }"""

   val testQuery3 = """PREFIX dbpedia: <http://dbpedia.org/resource/>
PREFIX dbprop:  <http://dbpedia.org/property/>
PREFIX dbpedia-owl:     <http://dbpedia.org/ontology/>

SELECT ?label, ?abstract, ?abstract_live, ?redirect, COUNT(?wikilink)
  WHERE {
    { <http://dbpedia.org/resource/Angelica_Kauffmann> dbpedia-owl:abstract ?abstract .
      FILTER ( langMatches( lang(?abstract), 'en') || ! langMatches (lang(?abstract),'*') ) .
    }
    UNION
    {
      <http://dbpedia.org/resource/Angelica_Kauffmann> <http://www.w3.org/2000/01/rdf-schema#label> ?label .
     FILTER ( langMatches( lang(?label), 'en') || ! langMatches(lang(?label),'*') ) .

    }
    UNION
    {
      <http://dbpedia.org/resource/Angelica_Kauffmann> dbpprop:abstract_live ?abstract_live .
     FILTER ( langMatches( lang(?abstract_live), 'en') || ! langMatches(lang(?abstract_live),'*') ) .

    }
    UNION
    {
      OPTIONAL {?redirect dbpprop:redirect <http://dbpedia.org/resource/Angelica_Kauffmann> .
                OPTIONAL { ?wikilink dbpprop:wikilink  ?redirect }
      }
    }
  }"""
  
  val testQuery4 = """SELECT DISTINCT  ?p ?o
WHERE
  { <http://dbpedia.org/resource/Air_Berlin> ?p ?o }
LIMIT   50"""

   val testQuery5 = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>PREFIX rdf:<http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX : <http://dbpedia.org/resource/>
PREFIX dbpedia2: <http://dbpedia.org/property/>
PREFIX dbpedia: <http://dbpedia.org/>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
SELECT * WHERE { <http://dbpedia.org/resource/3Com> ?pf1 ?middle . <http://dbpedia.org/resource/3Com> ?ps1 ?os1 . ?os1 ?ps2 ?middle . FILTER ((?middle != <http://dbpedia.org/resource/3Com> ) && (?middle != <http://dbpedia.org/resource/3Com> ) && (?middle != ?os1 ) && (?os1 != <http://dbpedia.org/resource/3Com> ) && (?os1 != <http://dbpedia.org/resource/3Com> ) && (?os1 != ?middle ) ). } LIMIT 20
"""

  val testQuery6 = """SELECT DISTINCT  ?s ?o
FROM <http://dbpedia.org>
WHERE
  { ?s <http://www.w3.org/2000/01/rdf-schema#label> ?o }
OFFSET  5334000
LIMIT   1000
"""
   
  val testQuery7 = """select * Where { 
      <http://dbpedia.org/resource/3Com> ?pf1 ?of1 . ?of1 ?pf2 ?middle . 
      <http://dbpedia.org/resource/3Com> ?ps1 ?middle . 
      fIlTeR ((?middle != <http://dbpedia.org/resource/3Com> ) && (?middle != <http://dbpedia.org/resource/3Com> ) && 
              (?middle != ?of1 ) && (!isLiteral(?of1)) && (?of1 != <http://dbpedia.org/resource/3Com> ) &&
              (?of1 != <http://dbpedia.org/resource/3Com> ) && (?of1 != ?middle ) ). } LIMIT 20"""
              
   val testQuery8 = """PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT * WHERE { ?city a <http://dbpedia.org/ontology/Place>; rdfs:label 'Krasnyy Kundul\''@de.  ?airport a <http://dbpedia.org/ontology/Airport>. {?airport <http://dbpedia.org/ontology/city> ?city} UNION {?airport <http://dbpedia.org/ontology/location> ?city} UNION {?airport <http://dbpedia.org/property/cityServed> ?city.} UNION {?airport <http://dbpedia.org/ontology/city> ?city. }{?airport <http://dbpedia.org/property/iata> ?iata.} UNION  {?airport <http://dbpedia.org/ontology/iataLocationIdentifier> ?iata. } OPTIONAL { ?airport foaf:homepage ?airport_home. } OPTIONAL { ?airport rdfs:label ?name. } OPTIONAL { ?airport <http://dbpedia.org/property/nativename> ?airport_name.} FILTER ( !bound(?name) || langMatches( lang(?name), 'de') )}"""

   val testQuery9 = """select distinct ?Concept where {[] a ?Concept} LIMIT 100"""
   
   SPARQLParser.parse(testQuery1)
   SPARQLParser.parse(testQuery2, Map("geo" -> "http://www.w3.org/2003/01/geo/wgs84_pos#",
              "foaf"->"http://xmlns.com/foaf/0.1/"))
   SPARQLParser.parse(testQuery3,Map(
              "dbpprop"->"http://dbpedia.org/property/"))
   SPARQLParser.parse(testQuery4)
   SPARQLParser.parse(testQuery5)
   SPARQLParser.parse(testQuery6)
   SPARQLParser.parse(testQuery7)
   SPARQLParser.parse(testQuery8,Map("geo" -> "http://www.w3.org/2003/01/geo/wgs84_pos#",
              "foaf"->"http://xmlns.com/foaf/0.1/"))
   SPARQLParser.parse(testQuery9)
}
