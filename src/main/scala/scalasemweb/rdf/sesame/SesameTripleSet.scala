/****************************************************************************
 * Copyright (c) 2011, Monnet Project
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Monnet Project nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE MONNET PROJECT BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ********************************************************************************/

package scalasemweb.rdf.sesame

import scalasemweb.rdf.model._
import org.openrdf.model.{Value=>SValue,Literal=>SLiteral,Resource=>SResource,URI=>SURI,_}
import org.openrdf.repository._

/**
 * 
 * @author John McCrae
 */
class SesameTripleSet(repo : Repository, context : Option[Resource] = None, pending : TripleSet = TripleSet()) extends TripleSet with SesameConversions {
  lazy val connection = repo.getConnection()
  
  protected override def finalize {
    connection.close
  }
  
  def has(subject : Option[Resource], predicate : Option[NamedNode], obj : Option[Value]) = {
    context match {
      case Some(ctxt) => connection.hasStatement(subject.getOrElse(null),predicate.getOrElse(null),obj.getOrElse(null),false,ctxt)
      case None => connection.hasStatement(subject.getOrElse(null),predicate.getOrElse(null),obj.getOrElse(null),false)
    }
    
  }
  def get(subject : Option[Resource], predicate : Option[NamedNode], obj : Option[Value]) = {
    new RepositoryResultTripleSet( () => {
        context match {
          case Some(ctxt) => connection.getStatements(subject.getOrElse(null),predicate.getOrElse(null),obj.getOrElse(null),false,ctxt)
          case None => connection.getStatements(subject.getOrElse(null),predicate.getOrElse(null),obj.getOrElse(null),false)
        }
      },pending,factory)
  }
  
  /**
   * Commit this instance to the sesame repository. Note this will change all other
   * instances tied to this repository... use with care!
   */
  def commit : SesameTripleSet = {
    val connection = repo.getConnection()
    try {
      for(triple <- pending) {
        context match {
          case Some(ctxt) => connection.add(triple,ctxt)
          case None => connection.add(triple)
        }
      }
      new SesameTripleSet(repo,context)
    } finally {
      connection.close()
    }
  }
  
  
  def +(triple : Triple) = {
    new SesameTripleSet(repo,context,pending + triple)
  }
 
  def -(triple : Triple) = {
    new SesameTripleSet(repo,context,pending - triple)
  }
  
  def iterator = get(None,None,None).iterator
  
  def contains(triple : Triple) = has(Some(triple.subj),Some(triple.pred),Some(triple.obj))
  
  lazy val factory = repo.getValueFactory()
}

trait SesameConversions {
  def factory : ValueFactory 
  
  implicit def tripToS(triple : Triple) : Statement = factory.createStatement(triple.subj,triple.pred,triple.obj)
  
  implicit def valToS(value : Value) : SValue =  value match {
    case null => null
    case lit : Literal => litToS(lit)
    case res : Resource => resToS(res)
  }
  
  implicit def litToS(literal : Literal) : SLiteral = literal match {
    case null => null
    case LangLiteral(value,lang) => factory createLiteral(value,lang)
    case TypedLiteral(value,typ) => factory createLiteral(value,nnToS(typ))
    case SimpleLiteral(value) => factory createLiteral(value) 
  }
  
  implicit def resToS(res : Resource) : SResource = res match {
    case null => null
    case nn : NamedNode => nnToS(nn)
    case BlankNode(id) => factory createBNode(id)
  }
  
  implicit def nnToS(nn : NamedNode) : SURI = if(nn != null) {
    factory createURI(nn.uri.toString())
  } else {
    null
  }
  
  implicit def statToSc(stat : Statement) : Triple = {
    stat.getSubject() %> stat.getPredicate() %> stat.getObject()
  }
  
  implicit def valToSc(value : SValue ) : Value = value match {
    case null => null
    case lit : SLiteral => litToSc(lit)
    case res : SResource => resToSc(res)
  }
  
  implicit def litToSc(lit : SLiteral) : Literal = if(lit == null) {
    null
  } else if(lit.getLanguage() != null) {
    LangLiteral(lit.getLabel(),lit.getLanguage())
  } else if(lit.getDatatype() != null) {
    TypedLiteral(lit.getLabel(),nnToSc(lit.getDatatype()))
  } else {
    SimpleLiteral(lit.getLabel())
  }
  
  implicit def resToSc(res : SResource) : Resource = res match {
    case null => null
    case bn : BNode => BlankNode(bn.getID())
    case nn : SURI => nnToSc(nn)
  }
  
  implicit def nnToSc(nn : SURI) : NamedNode = if(nn != null) {
    nn.stringValue().uri
  } else {
    null
  }
}

class RepositoryResultTripleSet(sesameIter : () => RepositoryResult[Statement], pending : TripleSet, val factory : ValueFactory) extends TripleSet with SesameConversions {
  def collapse = {
    val set = new scala.collection.mutable.HashSet[Triple]()
    val iter = sesameIter()
    while(iter.hasNext()) {
      set += iter.next()
    }
    set ++= pending
    TripleSet(set.toSeq:_*)
  }
  
  def has(subject : Option[Resource], predicate : Option[NamedNode], obj : Option[Value]) = collapse.has(subject,predicate,obj)
  
  def get(subject : Option[Resource], predicate : Option[NamedNode], obj : Option[Value]) = collapse.get(subject,predicate,obj)
  
  def -(statement : Triple) = collapse - statement
  def +(statement : Triple) = collapse + statement
  def ++(triples : TripleSet) = collapse ++ triples
  def --(triples : TripleSet) = collapse -- triples
  def contains(statement : Triple) = collapse contains statement
  
  def iterator = {
    new Iterator[Triple] {
      lazy val iter = sesameIter()
      lazy val pendIter = pending.iterator
      def hasNext = iter.hasNext() || pendIter.hasNext
      def next = if(iter.hasNext()) {
        iter.next()
      } else {
        pendIter.next
      }
    }
  }
}