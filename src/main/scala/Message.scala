/**
 * Created by shumonmadzhumder on 5/1/14.
 */
package threadbuilder
class Message(val subject: String, val messageId: String, val references: List[String]) {
  override def toString(): String = "(" + subject + ", " + messageId + ", " + references + ")";
}