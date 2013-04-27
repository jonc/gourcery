package jon.test

/**
 * this knows how represent hudson project changes as something that gource will understand
 */
object GourceWriter {

  /**
   * tell gource, by writing to STDOUT in custom log format, of a change in the project we are monitoring
   */
  def notifyGourceOfChange(c: Change) = Console.println(formatChange(c))

  /**
   * this formats a change object into something that gource will recognise
   */
  private def formatChange(change: Change): String = "%s|%s|%s|%s".format(change.when, change.who, change.how, change.what);

}