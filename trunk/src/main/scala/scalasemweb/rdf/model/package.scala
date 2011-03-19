package scalasemweb.rdf

import java.net.URI

package object model {
  /////////////////////////////////////////////////////////////////////////////////////////////
  // Sugaring
  /** Convert a string to a plain literal */
  implicit def str2lit(str:String) = SimpleLiteral(str)
  /** Convert a string to typed literal, lang literal or URI ref */
  implicit def str2vb(str:String) = new  {
    def ^^(typ:NamedNode) = TypedLiteral(str,typ)
    def @@(lang:String) = LangLiteral(str,lang)
    def uri = URIRef(URI.create(str))
  }
  /** Convert a symbol to a QName in the RDF base namespace */
  implicit def sym2qn(sym:Symbol) : NamedNode = RDF.base&(sym.name)
  
 // implicit def sym2res(sym:Symbol) : Resource = sym2qn(sym)
  
  /** Convert a URI to a URIref */
  implicit def uri2ref(uri : URI) = URIRef(uri)
  /** Convert a named node to a URI */
  implicit def ref2uri(ref : NamedNode) = ref.uri 
  /**
   * An anonymous blank RDF resource.
   */
  def AnonymousNode() = BlankNode(scala.math.abs((new java.util.Random()).nextLong()).toString())
}
