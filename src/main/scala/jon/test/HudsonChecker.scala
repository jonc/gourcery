package jon.test

import akka.actor.Props
import akka.actor.Actor

/**
 * companion object that will seed our instance with the user specified config it needs
 */
object HudsonChecker {
  def apply(c: Config): Props = Props(new HudsonChecker(c))
}

/**
 * this represents a change that the actor should tell gource about.
 * A change is a modification to a file in the project we are tracking
 */
case class Change(when: Long, who: String, what: String, how: String)

/**
 * An actor who knows how to talk to hudson over its json rest api and
 * write project changes to STDOUT in the format expected by the
 * gource custom log format
 */
class HudsonChecker(config: Config) extends Actor {

  /**
   * this understands the json that hudson supplies
   */
  private val parser = JsonParser(config)

  /**
   * this understands how to talk the gource custom log format
   */
  private val writer = GourceWriter

  /**
   * dirty rotten state - needed to remember where we got to last time we checked hudson for changes
   */
  var lastBuildProcessed: Int = config.initial

  /**
   * actor API - we got prompted to do something.
   * all we ever do is try to tell gource about any new changes on hudson that it should know about
   */
  def receive = {
    case _ => tellGourceAboutAnyNewChangesOnHudson
  }

  /**
   * try to ask hudson if it knows of any new changes that we need to report to gource.
   * If there are some new changes then tell gource about them
   */
  def tellGourceAboutAnyNewChangesOnHudson = {
    latestBuildNumOnHudson match {
      case Some(num: Int) => {
        // catch up with what we've missed
        getUnprocessedBuildsUpto(num) foreach tellGourceAboutTheChangesInABuild

        // and then remember where we got to
        lastBuildProcessed = num
      }
      // couldn't either talk to hudson at all, or understand what it was saying
      case _ => ??? //we should probably log this
    }
  }

  /**
   * try to ask hudson about all the changes in a particular build and tell gource about them
   */
  def tellGourceAboutTheChangesInABuild(buildNum: Int) = {
    val buildUrl = parser makeBuildUrlFrom buildNum
    for {
      change <- parser.retrieveChangesForBuild(getFileContents(buildUrl)).getOrElse(Nil)
    } yield {
      writer notifyGourceOfChange change
    }
  }

  /**
   * try to ask hudson what it thinks the last build on our project was
   */
  def latestBuildNumOnHudson: Option[Int] = parser.parseLastBuildNum(getFileContents(parser.projectUrl))

  /**
   * Helper - get the contents of a url as a string
   */
  private def getFileContents(projectUrl: String): String = io.Source.fromURL(projectUrl).getLines.mkString

  /**
   * Helper - range of builds from the first one we haven't told gource about to the build identified by the build number passed in
   */
  private def getUnprocessedBuildsUpto(num: Int) = {
    lastBuildProcessed + 1 to num
  }

}

