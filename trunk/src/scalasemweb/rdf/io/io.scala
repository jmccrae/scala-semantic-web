package scalasemweb.rdf.io

import scalasemweb.rdf.model._

/**
 * An object capable of writing a statement set to an output target
 */
trait RDFWriter {
  /**
   * Write a set of statements to a string
   * @param statSet The set of statements
   * @return The string in the format of the writer
   */
   def write(statSet : StatementSet[Statement]) : String 
   /**
    * Write a set of statements to a target
    * @param stat The set of statements
    * @param out The target for output
    */
   def write(statSet : StatementSet[Statement], out : Appendable) : Unit
}

/**
 * An object capable of reading RDF from a file or input source
 */
trait RDFParser {
  /**
   * Read a set of statements from a string
   * @param doc The data as a string (not a filename!)
   * @return The set of statements represented by the data
   * @throws RDFParseException If the document could not be parsed
   */
   @throws(classOf[RDFParseException])
  def parse(doc:String) : StatementSet[Statement]
  /**
   * Read a set of statements from a input source
   * @param in The input source
   * @return The set of statements read from the input source
   * @throws RDFParseException If the input stream could not be parsed
   */
   @throws(classOf[RDFParseException])
  def parse(in: java.io.Reader) : StatementSet[Statement]
}

class RDFParseException(msg : String = "", cause : Throwable = null) extends RuntimeException(msg,cause)
