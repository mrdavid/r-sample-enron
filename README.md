## Analyzing the Enron Email Corpus - Demo

This R file analyses some of the Enron Email Corpus. It produces 4 PDF files, each containing a graph
displaying how different persons are connected through emails present in the corpus.

The Enron Email Corpus can be downloaded from http://www.cs.cmu.edu/~enron/ and the contents
should be untared in the same directory as the R script, producing a folder named 'enron_mail_20110402'.
The first line of the R script needs to be adjusted to the environment. A script that does all of this automatically is provided (see below).

The R script itself contains more comments with details on each step in the analysis.

### Data selection

Due to constrains on time and resources, only a subset of the emails is analysed.

*Only emails from 'Sent' or 'Sent_Items' folders are analysed.*

This greatly reduces the number of emails. 'Sent' folders will not contain spam and will only
contain email sent on purpose by an Enron employee, making it uncessary to filter emails for
validity. 'Sent' folders will furthermore not contain duplicates (i.e. an email in the 'Sent'
folder of one person will be present in the inbox of another person, but not in any other
'Sent' folder), relieving us of the work of identifying such duplicates and removing them.

The 'Sent' type folders are filtered out with the following command:

```
find . -type d  | grep -i sent | grep -v presentation > sent_folders
``` 

*Only emails sent from one person to exactly one person are analysed*

Although the data set contains many emails sent to several receipients, parsing the *To:*
field of the emails was skipped for time constraints. Only emails sent directly to one
receipient are kept.

### Analysis: Mails sent between people whose mailboxes are in the data set

_Graph: 00.most.mails.pdf_

The first analysis looks only at emails that are sent between people whose mailbox is in the data set.
To do that, only emails are kept that are sent to someone that has also sent a mail him/herself.

The graph shows connections between people where more than 150 emails have been sent. This number
reduces the data set to only 21 people, a number that can still be plotted nicely. The thickness of
each arch in the graph shows the volume of email exchanged. The size of each vertex is proportional
to the number of different people that person has sent emails to overall.

Within the selected data, unfortunately most mails sent directly seem to be quite disconnected from each other.
The connection between Kay Mann and Suzanne Adams is exceptionally strong.

### Analysis: Important people

We quickly analyse some important people within the network. A paper by Shetty and Adibi suggests these names:

* Louise Kitchen
* Mike Grigsby
* Greg Whalley
* Scott Neal
* Kenneth Lay

#### Mails to or from important people: Whole network

_Graph:  01.important.people.pdf_

We first simply plot the whole network of people that have sent mails to or received mails from one of the members
in the list of important people. The graph shows some interesting properties. While there are a lot of people
that only exchange emails with one of the important people (this is probably in part due to their inboxes not being in the data set), there is a group of people (in the center of the graph) that is connected to nearly all of the important people. (E.g. John Lavorato, Jeffrey Shankman, etc)

#### Mails to or from important people: Network of 'well connected' people

_Graph: 02.important.people.well.connected.pdf_

The graph shows only those people in the network that have more than 4 connections. Within the selected data set, this will be people that are connected to most of the 'important people' in the network. One can see that nearly
everybody is connected to Louise Kitchen.

#### Mails to or from important people: Strength of connections between 'well connected' people

_Graph: 03.important.people.well.connected.weights.pdf_

The last graph shows the network of 'well connected' people with thickness of lines indicating the number of emails sent from one person to the other. The connection between Louise Kitchen and John Lavarato and Sally Beck is exceptionally strong. Furthermore, John Lavarato has quite strong connections to all of the 'important people' mentioned above.

## Running the analysis from scratch

The analysis can be run from scratch, using only the R script and assuming that the relevant libraries are installed by using

```
chmod u+x run.sh
./run.sh
```

### Sources

Source of Enron Corpus: http://www.cs.cmu.edu/~enron/