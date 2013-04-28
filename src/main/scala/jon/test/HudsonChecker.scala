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
  var lastBuildProcessed: Int = 0 // haven't checked at all yet

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
      
      // talked to hudson - we now know about the last build - so tell gource about any changes
      case Some(num: Int) => updateGource(num)
      
      // either couldn't talk to hudson at all, or couldn't understand what it was saying
      case _ =>  lastBuildProcessed match {
        case 0 => {
          // never been able to talk to hudson - something is wrong so give up
          Console.err.println("Can't communicate with the hudson server at " + config.repo );
          System.exit(-1);
        }
        case _ => // do nothing probably just a temporary glitch
      }
    }
  }
  
  /**
   * we talked to hudson and now know what the latest build was, 
   * so we should tell gource about all the changes we haven't told it about yet
   */
  private def updateGource(num: Int): Unit = {
    // catch up with what we've missed
    getUnprocessedBuildsUpto(num) foreach tellGourceAboutTheChangesInABuild

    // and then remember where we got to
    lastBuildProcessed = num
  }

  /**
   * try to ask hudson about all the changes in a particular build and tell gource about them
   */
  def tellGourceAboutTheChangesInABuild(buildNum: Int) = {
    val buildUrl = parser makeBuildUrlFrom buildNum
    getFileContents(buildUrl) map { buildJson =>
      for {
        change <- parser.retrieveChangesForBuild(buildJson).getOrElse(Nil)
      } yield {
        writer notifyGourceOfChange change
      }
    }
  }

  /**
   * try to ask hudson what it thinks the last build on our project was
   */
  def latestBuildNumOnHudson: Option[Int] = getFileContents(parser.projectUrl) match {
    case Some(projectJson) => parser.parseLastBuildNum(projectJson) // could get json from hudson, so use that to try and work out the build no
    case _ => None // couldn't get the json so we are doomed
  }

  /**
   * Helper - try to get the contents of a url as a string which contains json
   */
  private def getFileContents(url: String): Option[String] = {

    def isJson(str: String): Boolean = str.trim().startsWith("{") //for our purposes this is sufficient

    try {
      val str = io.Source.fromURL(url).getLines.mkString
      if (isJson(str)) Some(str) else None
    } catch {
      case _: Throwable => None
    }

  }

  /**
   * Helper - range of builds from the first one we haven't told gource about to the build identified by the build number passed in
   * if this is first time we process builds since we got started, then we need to determine where to start reporting from
   */
  private def getUnprocessedBuildsUpto(num: Int) = (lastBuildProcessed, config.initial) match {
    case (0, 0) => num to num // haven't started and haven't specified where to start from, so just this last one
    case (0, _) => config.initial to num // haven't started, but did specify where to start from, so use that range
    case (_, _) => lastBuildProcessed + 1 to num // already running so just report the ones we haven't already done
  }
}

