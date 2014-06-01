emailThreader is a small Scala library (.jar) which lets you order a list of E-Mails by conversation.
That is, grouping messages together in parent/child relationships based on which 
messages are replies to which others.

The library implements the JWZ E-Mail threading algorithm as described by Jamie Zawinski (http://www.jwz.org/doc/threading.html)
but omits the last step of the algorithm which is grouping by subject name

There is an IMAP extension Internet Draft available which decribes
the same here:
http://www.jwz.org/doc/draft-ietf-imapext-thread-12.txt


Install Scala >= 2.10

Install sbt

Run sbt clean test

Run sbt clean package



