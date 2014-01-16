package org.dbpedia.spotlight.jdbm.index

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import net.kotek.jdbm.DB


/**
 * This ScalaTest test if the JDBM index is valid.
 *
 * @author Alexandre Cançado Cardoso - accardoso
 */


@RunWith(classOf[JUnitRunner])
class JdbmIndexTest extends FlatSpec with ShouldMatchers {

  /* Test params */
  //The path of the server.properties file
  val indexingConfigFileName: String = "./conf/server.properties"

  /* JDBM Index Tests */

  "The Lucene Index" should "be valid" in {
    JDBMIndexTest.isIndexValid(indexingConfigFileName) should be === true
  }

}

object JDBMIndexTest {

  var indexDirectory: Any = null

  def isIndexValid(indexingConfigFileName: String): Boolean = {
    //TODO

    false
  }

}