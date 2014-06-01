/**
 * Created by shumonmadzhumder on 5/2/14.
 */
package threadbuilder
import scala.collection.mutable.HashMap

class Threading {

  def thread(messages : List[Message]) : Container = {
    val msgMap = createMsgMap(messages)
    val tree = new Container()
    msgMap.values.foreach(c => {
      if (!c.maybeParent.isDefined) {
        //if root
        tree += c
      }
    })
    pruneEmptyContainers(tree)
    tree
  }

  def createMsgMap(messages: List[Message]): HashMap[String, Container] = {
    val h = new HashMap[String, Container]
    messages.foreach(m => {
      // 1A retrieve container or create a new one
      val parent = h.getOrElseUpdate(m.messageId, new Container())
      parent.maybeMessage = Some(m)
      /*
      *  Link the References field's Containers together in the
      *  order implied by the References header
      *
      * 1B
      # for each element in the message's references field find a container or create a new one
      * */

      val last = m.references.foldLeft(None : Option[Container])((previous, current) => {

        val container = h.getOrElseUpdate(current, new Container())
        previous.foreach(p => {
          //avoid circular loop
          if(container.maybeParent.isEmpty && !container.hasDescendant(Some(p))) {
            p += container
          }
        })
        Some(container)
      })

      // C Set the parent of this message to be the last element in References
      if(last.isDefined && !parent.hasDescendant(last)) {
        last.foreach(_ += parent)
      }
    })
    h
  }

  def pruneEmptyContainers(tree: Container): Unit = {

    tree.children.reverseIterator.foreach(container => {
      pruneEmptyContainers(container)
      if (container.isDummy() && container.children.isEmpty) {
        //it is a dummy message without children, delete it
        tree -= container
      }
      else if (container.isDummy()) {
        /* it is a dummy message with children, delete it, BUT!
           Do not promote the children if doing so would make them
           children of the root, unless there is only one child.
        */
        if (!tree.maybeParent.isDefined && container.children.size == 1) {
          tree.promoteOneLevelUp(container)
        }
        else if(!tree.maybeParent.isDefined) {

        }
        else {
          tree.promoteOneLevelUp(container);
        }
      }
    })
  }
}