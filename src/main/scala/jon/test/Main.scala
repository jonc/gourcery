package jon.test

import scopt.immutable.OptionParser
import akka.actor.Actor
import akka.actor.Props
import scala.concurrent.duration._
import akka.actor.ActorSystem

case class Config(
  project: String = "2_4_SSLVPN",
  delay: Int = 30,
  repo: String = "http://10.14.0.199:8080/jenkins",
  initial: Int = 70) 
  
  
object Main extends App {

  val parser = new OptionParser[Config]("gourcery", "1.0") {
    def options = Seq(
      opt("p", "project", "The project in Hudson to point at") {
        (p: String, c: Config) => c.copy(project = p)
      },
      opt("r", "repo", "the URL of the repository to query") {
        (p: String, c: Config) => c.copy(project = p)
      },
      intOpt("d", "delay", "num seconds to wait between checks") {
        (x: Int, c: Config) => c.copy(delay = x)
      },
      intOpt("i", "initial", "the build number to start from") {
        (x: Int, c: Config) => c.copy(initial = x)
      })
  }

  parser.parse(args, Config()) map { config =>

    val system = ActorSystem("mySystem")
    val tickActor = system.actorOf(HudsonChecker(config))

    //Use system's dispatcher as ExecutionContext
    import system.dispatcher

    // trigger the actor to check hudson for changes on
    // the schedule the user specified
    system.scheduler.schedule(0 milliseconds,
      config.delay seconds,
      tickActor,
      "ping")

  } getOrElse {
    Console.println("exiting")
  }
}