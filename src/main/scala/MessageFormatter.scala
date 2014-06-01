/**
 * Created by shumonmadzhumder on 5/2/14.
 */
package threadbuilder
import scala.collection.mutable.ListBuffer

object MessageFormatter {

  def format(subject : String, messageId : String, inReplyTo :List[String], references : ListBuffer[String]) : Message = {

      /*
       if there are no message-IDs in references header
        use the first found message-ID of the in-reply-to header instead
      */

      if(!inReplyTo.isEmpty && !references.contains(inReplyTo.head)) {
        references ++= List(inReplyTo.head)
      }
      //class Message(val subject: String, val messageId: String, val references: List[String]) {
      new Message(subject, messageId, references.toList)
  }
}
