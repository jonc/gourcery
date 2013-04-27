package jon.test

import scala.util.parsing.json.JSON

object JsonParser {
  def apply(config: Config) = new JsonParser(config)
}

/**
 * this class understands how to get the info we need to tell gource about out of various calls into the json api provided by hudson
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

    // helper - this turns each map in the item list into a Change object
    def parseChange(c: JsonMap): Seq[Change] = {

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
    // work through all the items in the change set - the item map contains the commits
    buildJson(str) map { json =>
      val items = json("changeSet").asInstanceOf[JsonMap]("items").asInstanceOf[JsonArray]
      items.foldLeft(Seq[Change]()) {
        (r: Seq[Change], c: JsonMap) => r ++ parseChange(c) //keep in order, add newer ones on the end
      }
    }
  }
}