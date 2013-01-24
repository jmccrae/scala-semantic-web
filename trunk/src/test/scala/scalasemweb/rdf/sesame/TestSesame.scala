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

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scalasemweb.rdf.model._
import scalasemweb.rdf.sesame._

/**
 * 
 * @author John McCrae
 */
class TestSesame extends FlatSpec with ShouldMatchers {
  val repo = new org.openrdf.repository.sail.SailRepository(new org.openrdf.sail.memory.MemoryStore())
  repo.initialize;
  def runTest {
    val connection = repo.getConnection()
    (connection.hasStatement(null,null,null,false)) should be (false)
    val sts = new SesameTripleSet(repo)
    (sts has (None,None,None)) should be (false)
    sts should be ('empty)
    RDF.base = "file:test#"
    val sts2 = sts + ('test %> 'test %> 'test)
    sts2 should not be ('empty)
    sts2.commit
    (sts has (None,None,None)) should be (true)
    for(stat <- sts get (None,None,None)) {
      println(stat)
    }
    sts should not be ('empty)
    connection.close
    sts.close
  }
  runTest
  repo.shutDown()
   
}
