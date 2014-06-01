/**
 * Created by shumonmadzhumder on 5/1/14.
 */
package threadbuilder
import scala.collection.mutable.ListBuffer

class Container {

  var maybeParent = None : Option[Container]
  val children = new ListBuffer[Container]()
  var maybeNext = None : Option[Container]
  var maybeMessage = None : Option[Message]

  def this(message : Option[Message]) = {
    this()
    maybeMessage = message
  }
  
  def isDummy() : Boolean = {
    !maybeMessage.isDefined
  }

  def -=(child : Container) : Unit = {
        children --= List(child)
        child.maybeParent = None
  }

  def +=(child : Container) : Unit = {
    child.maybeParent.foreach(p => {
      p -= child
    })
    children ++= List(child)
    child.maybeParent = Some(this)
  }

  def promoteOneLevelUp(container : Container) : Unit = {
    container.children.reverseIterator.foreach(c => {
      +=(c)
    })
    -=(container)
  }

  def hasDescendant(maybeContainer : Option[Container]) : Boolean = {
    def stop(other: Container): Boolean = {
      if(children.isEmpty) false
      if(this eq other) true
      false
    }
    maybeContainer match {
      case None => false
      case Some(c) => if(stop(c)) true
        children.foreach((child: Container) => child.hasDescendant(maybeContainer))
        false
    }
  }
}

