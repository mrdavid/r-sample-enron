Source of Enron Corpus: http://www.cs.cmu.edu/~enron/

Command to filter only emails in the 'sent' folders:
find . -type d  | grep -i sent

FROM all mails in all mailboxes
  TAKE ONLY mails in folders containing the word 'sent'
    (REASON: 'Sent' folders should only contain messages form the owner of the mailbox - keeping the list to people we have data about)
  TAKE ONLY mails that are have been sent to ONLY ONE PERSON
    (REASON: ...)
  TAKE ONLY mails that have been sent TO someone that has also sent a mail
    (REASON: This way we should only get messages between people in the mailboxes)
  TAKE ONLY mails from from people that have exchanged more than 150 mails
    (REASON: This takes it down to 22 people, making for a nice plot)

Conclusion:
Vincent Kaminski forwards a his mail to his AOL account
https://en.wikipedia.org/wiki/Vincent_Kaminski
