package jon.test

import scopt.immutable.OptionParser
import akka.actor.Actor
import akka.actor.Props
import scala.concurrent.duration._
import akka.actor.ActorSystem

case class Config(
  project: String = null,
  delay: Int = 30,
  repo: String = null,
  initial: Int = 0,
  today: Boolean = false)

object Main extends App {

  val parser = new OptionParser[Config]("gourcery", "1.0") {
    def options = Seq(
      opt("p", "project", "The project in Hudson to point at") {
        (p: String, c: Config) => c.copy(project = p)
      },
      opt("r", "repo", "the URL of the repository to query") {
        (r: String, c: Config) => c.copy(repo = r)
      },
      intOpt("d", "delay", "num seconds to wait between checks") {
        (x: Int, c: Config) => c.copy(delay = x)
      },
      intOpt("i", "initial", "the build number to start from (defaults to last build if not specified)") {
        (x: Int, c: Config) => c.copy(initial = x)
      },
      flag("t", "today", "just show todays changes") { _.copy(today = true) },
      help("?", "help", "Show a usage message and exit"))
  }

  parser.parse(args, Config()) map { config =>
    // are we good to go?
    config match {
      case _ if (config.project == null || config.repo == null) => {
        // no we're not, so give up
    	Console.err.println("Both the name of the project and the url of the hudson server need to be specified on the command line")
    	System exit -1
      }
      case _ => {
        // yes we are, so spark it up
        val system = ActorSystem("gourcery")
        val hudsonChecker = system.actorOf(HudsonChecker(config))

        //Use system's dispatcher as ExecutionContext
        import system.dispatcher

        // trigger the actor to check hudson for changes on
        // the schedule the user specified
        system.scheduler.schedule(0 milliseconds,
          config.delay seconds,
          hudsonChecker,
          "ping")
      }
    }
  } getOrElse {
    Console.println("exiting")
  }
}