/*
Copyright (c) 2013 Jon Cundill, unless otherwise specified.
All rights reserved.

Permission to use, copy, modify, and distribute this software in source
or binary form for any purpose with or without fee is hereby granted,
provided that the following conditions are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

   3. Neither the name of the gourcery nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.


THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.
 */
package jon.test

import scala.util.parsing.json.JSON

object JsonParser {
  def apply(config: Config) = new JsonParser(config)
}

/**
 * this class understands how to get the info we need to tell gource about all of the various calls into the json api provided by hudson
 */
class JsonParser(config: Config) {

  type JsonMap = Map[String, Any]
  type JsonArray = Seq[JsonMap]
  
  def makeBuildUrlFrom(buildNum: Int) = config.repo + "/job/" + config.project + "/" + buildNum + "/api/json"

  def projectUrl = config.repo + "/job/" + config.project + "/api/json"

  /**
   * helper - try to build a json map out of the passed in string
   */
  private def buildJson(str: String): Option[JsonMap] = JSON.parseFull(str).asInstanceOf[Option[JsonMap]]

  /**
   * given a string that should represent the json of the project page, try to work out what the latest build number is
   */
  def parseLastBuildNum(str: String): Option[Int] = buildJson(str) map { json =>
    json("builds").asInstanceOf[JsonArray].head("number").asInstanceOf[Double].intValue()
  }

  /**
   * given a string that should represent the json for a particular build, try to parse all the changes in that build and return them
   */
  def retrieveChangesForBuild(str: String): Option[Seq[Change]] = {

    // helper - this turns each file in a single commit into a Change object
    def parseChanges(c: JsonMap): Seq[Change] = {

      // helper - to put the change type in the format gource wants
      def parseType(x: String): String = x match {
        case "edit" => "M"
        case "add" => "A"
        case "delete" => "D"
        case _ => throw new Exception(x)
      }

      val time = c("timestamp").asInstanceOf[Double].longValue() / 1000 // its in millis - we want seconds
      val author = c("author").asInstanceOf[JsonMap]("fullName").toString
      c("paths").asInstanceOf[JsonArray].foldLeft(Seq[Change]()) {
        (r: Seq[Change], p: JsonMap) => Change(time, author, p("file").toString, parseType(p("editType").toString)) +: r
      }
    } // end helper

    // algo starts here ...
    // work through all the items in the change set - the item list contains the commits
    buildJson(str) map { json =>
      val items = json("changeSet").asInstanceOf[JsonMap]("items").asInstanceOf[JsonArray]
      items.foldLeft(Seq[Change]()) {
        (r: Seq[Change], c: JsonMap) => r ++ parseChanges(c) //keep in order, add newer ones on the end
      }
    }
  }
}