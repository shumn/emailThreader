/**
 * Created by shumonmadzhumder on 5/3/14.
 */
import org.scalatest.FlatSpec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import threadbuilder.MessageFormatter
import threadbuilder.Container
import threadbuilder.Message
import threadbuilder.Threading

class ThreadingSpec extends FlatSpec {

  /*TO DO*/
  /* The References field is populated from the ``References'' and/or ``In-Reply-To'' headers.
  If both headers exist, take the first thing in the In-Reply-To header that looks like a Message-ID,
  and append it to the References header.
  */
  "test" should "create valid message by using reference field" in {

   val message = MessageFormatter.format("subject", "messageId", List("a"), ListBuffer("a", "c"))
   assert(ListBuffer("a", "c") === message.references)
  }

  "test" should "create valid message by using in-reply-to field" in {

    val message = MessageFormatter.format("subject", "messageId", List("a"), ListBuffer())
    assert(ListBuffer("a") === message.references)
  }

  "test" should "create valid message by using in-reply-to field with multiple message-IDs" +
                " but taking only the first message-ID into account" in {

    val message = MessageFormatter.format("subject", "messageId", List("a", "c"), ListBuffer())
    assert(ListBuffer("a") === message.references)
  }
  /*
  * Helper methods
  */
  def childMessageId(msgMap: HashMap[String, Container], msgId: String, index: Int) : String = {
      msgMap.get(msgId).get.children(0).maybeMessage.get.messageId
  }

  def childCount(msgMap: HashMap[String, Container], msgId: String) : Int = {
    msgMap.get(msgId).get.children.size
  }
  /*

   a
   +- b
      +- c
         +- d
            +- e
   b
   +- c
      +- d
         +- e
   c
   +- d
      +- e
   d
   +- e
   e
  */
  "test" should "create id_table for each message" in {
    val messages = List(new Message("subject", "a", List()),
                        new Message("subject", "b", List("a")),
                        new Message("subject", "c", List("a", "b")),
                        new Message("subject", "d", List("a", "b", "c")),
                        new Message("subject", "e", List("d")))

    val msgMap = new Threading().createMsgMap(messages)

    assert(5 === msgMap.size)
    assert("b" === childMessageId(msgMap, "a", 0))
    assert("c" === childMessageId(msgMap, "b", 0))
    assert("d" === childMessageId(msgMap, "c", 0))
    assert("e" === childMessageId(msgMap, "d", 0))
    assert(0 === childCount(msgMap, "e"))
  }

  /*
   a
   +- b
      +- c (dummy)
         +- d
           +- e
   b
   +- c (dummy)
      +- d
         +- e
   c (dummy)
   +- e
      +- e
   d
   +- e
   e:subject
  */
  "test" should "create message mappings for each message and dummy containers in case of" +
                " reference to non-existent message" in {
      val messages = List(new Message("subject", "a", List()),
                          new Message("subject", "b", List("a")),
                          //new Message("subject", "c", List("a", "b")),
                          new Message("subject", "d", List("a", "b", "c")),
                          new Message("subject", "e", List("d")))

      val msgMap = new Threading().createMsgMap(messages)
      assert(5 === msgMap.size)
      assert("b" === childMessageId(msgMap, "a", 0))
      assert(true == msgMap.get("c").get.isDummy())
      assert("d" === childMessageId(msgMap, "c", 0))
      assert("e" === childMessageId(msgMap, "d", 0))
      assert(0 == childCount(msgMap, "e"))

  }
  /*
   a
   +- b
     +- c (dummy)
         +- d
            +- e
   b
   +- c
      +- d
         +- e
   y (dummy)
   c
   +- d
      +- e
   z  (dummy)
   +- y (dummy)
   d
   +- e
   e
  */
  "test" should "create id_table for each message and nested dummy containers in case of" +
                " references to non-existent messages" in {
      val messages = List(new Message("subject", "a", List()),
                          new Message("subject", "b", List("a")),
                          new Message("subject", "d", List("a", "b", "c")),
                          new Message("subject", "e", List("z", "y", "d")))

      val msgMap = new Threading().createMsgMap(messages)
      assert(7 === msgMap.size)
      assert("b" === childMessageId(msgMap, "a", 0))
      assert(true == msgMap.get("c").get.isDummy())
      assert("d" === childMessageId(msgMap, "c", 0))
      assert(true == msgMap.get("z").get.isDummy())
      assert(true == msgMap.get("y").get.isDummy())
      assert(0 == childCount(msgMap, "e"))
      assert(0 == childCount(msgMap, "e"))
      assert("e" === childMessageId(msgMap, "d", 0))
  }
  /*
    before:
    a
    +- b
     +- dummy

   after:
   a
   +- b
  */
  "test" should "prune containers with empty message and no children" in {
    val tree = new Container()
    val msgA = new Message("subject", "a", List())
    val msgB = new Message("subject", "b", List("a"))

    val conA = new Container(Some(msgA))
    val conB = new Container(Some(msgB))
    val conDummy = new Container(None)

    tree += conA
    conA += conB
    conB += conDummy

    new Threading().pruneEmptyContainers(tree)

    assert(conA === tree.children.head)
    assert(1 === conA.children.size)
    assert(conB === conA.children.head)
    assert(0 === conB.children.size)
  }

  /*
   before:
   a
   +- b
      +- z (dummy)
         +- c

   after:
   a
   +- b
      +- c
  */
  "test" should "prune containers with empty message and 1 non-empty child" in {
    val tree = new Container()

    val msgA = new Message("subject", "a", List())
    val msgB = new Message("subject", "b", List("a"))
    val msgC = new Message("subject", "c", List("a", "z"))

    val conA = new Container(Some(msgA))
    val conB = new Container(Some(msgB))
    val conC = new Container(Some(msgC))
    val conDummy = new Container(None)

    tree += conA
    conA += conB
    conB += conDummy
    conDummy += conC

    new Threading().pruneEmptyContainers(tree)

    assert(conA === tree.children.head)
    assert(conB === conA.children.head)
    assert(conC === conB.children.head)
    assert(1 === tree.children.size)
    assert(1 === conB.children.size)
    assert(0 === conC.children.size)
  }

  /*
   before:
   a
   z (dummy)
   +- b

   after:
   a
   b
  */
  "test" should "promote child of containers with empty message and 1 child directly to root level" in {
    val tree = new Container()

    val msgA = new Message("subject", "a", List())
    val msgB = new Message("subject", "b", List("z"))


    val conA = new Container(Some(msgA))
    val conB = new Container(Some(msgB))
    val conDummy = new Container(None)

    tree += conA
    tree += conDummy
    conDummy += conB

    new Threading().pruneEmptyContainers(tree)

    assert(2 === tree.children.size)
    assert(conA === tree.children.head)
    assert(conB === tree.children(1))
  }
  /*
   before:
   a
   z (dummy)
   +- b
   +- c

   after:
   a
   z (dummy)
   +- b
   +- c
  */
  "test" should "do not promote children of containers with empty message and 2 children directly to root level" in {
    val tree = new Container()

    val msgA = new Message("subject", "a", List())
    val msgB = new Message("subject", "b", List("a", "z"))
    val msgC = new Message("subject", "c", List("a", "z"))

    val conA = new Container(Some(msgA))
    val conB = new Container(Some(msgB))
    val conC = new Container(Some(msgC))
    val conDummy = new Container(None)

    tree += conA
    tree += conDummy
    conDummy += conB
    conDummy += conC

    new Threading().pruneEmptyContainers(tree)

    assert(2 === tree.children.size)
    assert(conA === tree.children.head)
    assert(conDummy === tree.children(1))
    assert(2 === conDummy.children.size)
    assert(conB === conDummy.children(0))
    assert(conC === conDummy.children(1))
  }
  /*
   before:
   a
   z (dummy)
   +- y (dummy)
      +- b
      +- c
      +- d

   after:
   a
   z (dummy)
   +- b
   +- c
   +- d
  */
  "test" should "promote children of containers with empty message and 2 children directly to next level" in {
    val tree = new Container()

    val msgA = new Message("subject", "a", List(""))
    val msgB = new Message("subject", "b", List("a", "z"))
    val msgC = new Message("subject", "c", List("a", "z"))
    val msgD = new Message("subjecd", "d", List("a", "z"))

    val conA = new Container(Some(msgA))
    val conB = new Container(Some(msgB))
    val conC = new Container(Some(msgC))
    val conD = new Container(Some(msgD))
    val conZ = new Container(None)
    val conY = new Container(None)

    tree += conA
    tree += conZ
    conZ += conY
    conY += conB
    conY += conC
    conY += conD

    new Threading().pruneEmptyContainers(tree)

    assert(2 === tree.children.size)
    assert(3 === tree.children(1).children.size)
    assert(conA === tree.children.head)
    assert(true === tree.children(1).isDummy())
    assert(conD === tree.children(1).children(0))
    assert(conC === tree.children(1).children(1))
    assert(conB === tree.children(1).children(2))
  }
  /*
   before:
   a
   z (dummy)
   +- y (dummy)
      +- x (dummy)
         +- b
         +- c
   +- d

   after:
   a
   z (dummy)
   +- b
   +- c
   +- d
  */
  "test" should "promote children of several containers with empty message and 2 children directly to next level" in {
    val tree = new Container()

    val msgA = new Message("subject", "a", List(""))
    val msgB = new Message("subject", "b", List("a", "z"))
    val msgC = new Message("subject", "c", List("a", "z"))
    val msgD = new Message("subject", "d", List("a", "z"))

    val conA = new Container(Some(msgA))
    val conB = new Container(Some(msgB))
    val conC = new Container(Some(msgC))
    val conD = new Container(Some(msgD))
    val conZ = new Container(None)
    val conY = new Container(None)
    val conX = new Container(None)

    tree += conA
    tree += conZ
    conZ += conY
    conY += conX
    conX += conB
    conX += conC
    conZ += conD

    new Threading().pruneEmptyContainers(tree)

    assert(2 === tree.children.size)
    assert(conA === tree.children.head)
    assert(true === tree.children(1).isDummy())
    assert(3 === conZ.children.size)
    assert(conD == conZ.children(0))
    assert(conB === conZ.children(1))
    assert(conC === conZ.children(2))
  }
  /* before:
   z (dummy)
   +- y (dummy)
      +- a
   +- x (dummy)
   after:
   a
  */
  "test" should "promote children of several containers with empty message and multiple children" in {
    val tree = new Container()

    val msgA = new Message("subject", "a", List(""))

    val conA = new Container(Some(msgA))
    val conZ = new Container(None)
    val conY = new Container(None)
    val conX = new Container(None)

    tree += conZ
    conZ += conY
    conY += conA
    conZ += conX

    new Threading().pruneEmptyContainers(tree)

    assert(1 == tree.children.size)
    assert(conA == tree.children.head)
    assert(0 == conA.children.size)

  }
  /*
   before:
   z (dummy)
   +- y (dummy)
      +- x (dummy)
         +- w (dummy)
            +- a
               +- b
            +- c
               +- d
   +- v

   after:
   z (dummy)
   +- a
      +- b
   +- c
      +- d
  */
  "test" should "promote children of several containers with empty message and multiple children 2" in {
    val tree = new Container()

    val msgA = new Message("subject", "a", List())
    val msgB = new Message("subject", "b", List())
    val msgC = new Message("subject", "c", List())
    val msgD = new Message("subjecd", "d", List())

    val conA = new Container(Some(msgA))
    val conB = new Container(Some(msgB))
    val conC = new Container(Some(msgC))
    val conD = new Container(Some(msgD))

    val conZ = new Container(None)
    val conY = new Container(None)
    val conX = new Container(None)
    val conW = new Container(None)
    val conV = new Container(None)

    tree += conZ

    conZ += conY
    conY += conX
    conX += conW
    conW += conA
    conA += conB
    conW += conC
    conC += conD
    conZ += conV

    new Threading().pruneEmptyContainers(tree)

    assert(1 === tree.children.size)
    assert(conZ === tree.children.head)
    assert(2 === conZ.children.size)
    assert(conC === conZ.children.head)
    assert(conA === conZ.children(1))
    assert(conB === conA.children.head)
    assert(conD === conC.children.head)
  }
  /*
   before:
   z (dummy)
   +- y (dummy)
      +- x (dummy)
         +- w (dummy)
            +- a
               +- b
            +- c
               +- d
      +- v
         +- u
            +- t
               +- s
                  +- q
                     +- e
            +- p
               +- f

   after:
   z (dummy)
   +- a
      +- b
   +- c
      +- d
   +- e
   +- f
  */
  "test" should "promote children of several containers with empty message and multiple children 3" in {
    val tree = new Container()

    val msgA = new Message("subject", "a", List())
    val msgB = new Message("subject", "b", List())
    val msgC = new Message("subject", "c", List())
    val msgD = new Message("subject", "d", List())
    val msgE = new Message("subject", "e", List())
    val msgF = new Message("subject", "f", List())

    val conA = new Container(Some(msgA))
    val conB = new Container(Some(msgB))
    val conC = new Container(Some(msgC))
    val conD = new Container(Some(msgD))
    val conE = new Container(Some(msgE))
    val conF = new Container(Some(msgF))

    val conZ = new Container(None)
    val conY = new Container(None)
    val conX = new Container(None)
    val conW = new Container(None)
    val conV = new Container(None)
    val conU = new Container(None)
    val conT = new Container(None)
    val conS = new Container(None)
    val conQ = new Container(None)
    val conP = new Container(None)

    tree += conZ

    conZ += conY
    conY += conX
    conX += conW
    conW += conA
    conA += conB
    conW += conC
    conC += conD
    conZ += conV
    conV += conU
    conU += conT
    conT += conS
    conT += conQ
    conQ += conE
    conU += conP
    conP += conF


    new Threading().pruneEmptyContainers(tree)

    assert(1 === tree.children.size)
    assert(conZ === tree.children.head)
    assert(4 === conZ.children.size)
    assert(conF === conZ.children.head)
    assert(conE === conZ.children(1))
    assert(conC === conZ.children(2))
    assert(conA === conZ.children(3))
    assert(conB === conA.children.head)
    assert(conD === conC.children.head)

  }
}