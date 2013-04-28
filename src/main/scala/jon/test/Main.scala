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