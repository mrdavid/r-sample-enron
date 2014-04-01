setwd("/Users/davidr/enron/")

# This R script 
#  * requires the library "plyr"
#  * requires 'arcdiagram' which needs to be downloaded from github. Use these commands:
#    > library(devtools)
#    > install_github("arcdiagram", username = "gastonstat")
# You need to have the library "devtools" installed for this.
library(plyr)
library(arcdiagram)
require(igraph)

# Function to read a single email message's headers from a file.
# Adapted from Conway, White: Machine Learning for Hackers
get.message.headers  <- function(path){
  tryCatch({
    con <- file(path, open="rt", encoding="ascii")
    text <- readLines(con)
    # Headers of an email end with the first empty line in the file.
    headers <- text[seq(1,which(text=="")[1]-1, 1)]
    close(con)
    return(headers)
  },
  error = function(e){
    # Return NA if there was an error. A proper script would need error handling here.
    return(NA)
  })
}

# This function extracts the "To:" and "From:" fields from email headers.
parse.message <- function(headers){
  if(is.na(headers)){
    return(c(NA, NA))
  }
  
  from <- grep("^from:",headers,ignore.case=TRUE,value=TRUE)[1]
  # Just keep the email address without the From: field
  from <- sub("^From:\\s*", "", from, ignore.case=TRUE) 
  
  to <- grep("^to:",headers,ignore.case=TRUE,value=TRUE)[1]
  # Just keep the email address without the To: field
  to <- sub("^to:\\s*", "", to, ignore.case=TRUE)
  
  # If the email was sent to several receipients, they are separeted by commata and appear
  # on several consecutive lines.
  # This would require a proper parser. Given the time constraints while developing this script,
  # we throw away emails that have more than one receipient at this point.
  if(!is.na(to)){
    if(regexpr(",",to,fixed=TRUE)[1] != -1){
      to <- NA
    }
  }
  return(c(from, to))
}

# Get all 'Sent' folders from the mailboxes contained in the dataset
mail_folders <- file("sent_folders", open="rt", encoding="ascii")
mail_files_paths <- readLines(mail_folders)
close(mail_folders)

# Construct paths for all files contained in the above folders
mail_files <- unlist(lapply(mail_files_paths, function(path){
  paste(path, dir(path), sep="/")
}))
# Read and parse all mails (this step takes time)
mails <- sapply(mail_files, function(filename){ 
  parse.message(get.message.headers(filename))
})

# Turn mail data into an R data frame, removing cases that could not be parsed above.
mails <- na.omit(data.frame(mails[1,], mails[2,]))
colnames(mails) <- c("From", "To")

#####################################################################################
# We first limit the dataset to mails that are sent only between people whose mailbox
# is contained in the dataset. We assume that mails in the 'Sent' folder of a mailbox
# contain only such mails.
#
# We then plot a graph showing the mailboxes with the most emails exchanged and how
# (or if) they are connected.

# Get all mail addresses from which mails originated.
mails.sender <- unique(mails$From)
# Keep only those mails that were sent to one of these people.
mails.connected <- subset(mails, To %in% mails.sender)
# Count how many mails were sent from Person A to Person B. This will create a
# data frame with entries $From $To $weight where $weight is the number
# of mails sent $From -> $To
mails.counted <- ddply(mails.connected, .(From, To), summarise, weight = length(To))
# Remove @enron.com domain for better readability
mails.counted$From <- sub("@enron.com", "",mails.counted$From)
mails.counted$To <- sub("@enron.com", "",mails.counted$To)

# Produce a graph from these connections.
g0 <- graph.data.frame(mails.counted,directed=TRUE)
# Produce an undirected graph from g0, combining edge weights. We are only interested
# in how many mails have been sent in total between two people, not who send how many.
g <- as.undirected(g0, mode="collapse")
# Remove all connections where fewer than 150 emails have been sent.
gg <- delete.edges(g, which(E(g)$weight < 150))
# Remove vertices that are now disjunct. This leaves us with 21 nodes, which is still nicely plottable.
gg <- delete.vertices(gg, which(degree(gg) < 1))

# Plot this as a graph to a pdf file.
pdf("00.most.mails.pdf")
arcplot(get.edgelist(gg),height=5,width=5,unit="in", lwd.arcs = E(gg)$weight/100, cex.labels = 0.75,pch.nodes = 21, lwd.nodes = 2, line = -0.7, cex.nodes = degree(gg)*0.5)
dev.off()

#####################################################################################
# Now we try to learn something about 'important' people in the dataset.
# Looking at Shetty, Adibi - 'Discovering Important Nodes through Graph Entropy
# The Case of Enron Email Database'
# (cf: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.92.2782&rep=rep1&type=pdf )
# we simply take their "most important nodes of length 1" (see Table 1).

# Important people according to the paper cited above
important.people <- c("louise.kitchen@enron.com", "mike.grigsby@enron.com", "greg.whalley@enron.com", "scott.neal@enron.com", "kenneth.lay@enron.com")
# Keep only mails that are either From or To the "important" people.
mails.important <- subset(mails, From %in% important.people | To %in% important.people)
# Count how many mails were exchanged between each member in the data set.
mails.important.counted <- ddply(mails.important, .(From, To), summarise, weight = length(To))
# Remove enron domain for better readability
mails.important.counted$From <- sub("@enron.com", "",mails.important.counted$From)
mails.important.counted$To <- sub("@enron.com", "",mails.important.counted$To)

# Produce a graph from these connections
g <- graph.data.frame(mails.important.counted,directed=TRUE)
# This function produces a nice layout for plotting
l <- layout.fruchterman.reingold(g,niter=550,area=vcount(g)^2.3,repulserad=vcount(g)^2.8)

# Produce a plot showing the network.
pdf("01.important.people.pdf", height=34, width=34)
#arcplot(as.matrix(mails.important.counted.10)[,1:2], lwd.arcs = mails.important.counted.10$weight/10, cex.labels = 0.5,pch.nodes = 20, lwd.nodes = 1, line = -0.2)
plot.igraph(g, layout=l, vertex.size=0)
dev.off()

# We can see from the above graph that, while there are many people that exchange mails
# with only one of the "important" people selected above, there are some individuals
# which exchange mails with nearly all of them. We try to look at people that are connected
# with at least 4 other people, which in this graph will be people connected to
# at least 4 of the "important" people.

# Remove alls nodes which are connected to less than 5 other nodes.
gg2 <- delete.vertices(g, which(degree(g) <5))
# Print to a PDF
pdf("02.important.people.well.connected.pdf", height=20, width=20)
#arcplot(as.matrix(mails.important.counted.10)[,1:2], lwd.arcs = mails.important.counted.10$weight/10, cex.labels = 0.5,pch.nodes = 20, lwd.nodes = 1, line = -0.2)
plot.igraph(gg2, layout=layout.circle, vertex.size=0,vertex.label.cex=4,margin=0.1)
dev.off()

#gg3 <- delete.edges(gg2, which(E(gg)$weight < 1))
#gg3 <- delete.vertices(gg3, which(degree(gg) < 1))

# And now we try to find out where most mails were sent in this reduced graph:
pdf("03.important.people.well.connected.weights.pdf")
arcplot(get.edgelist(gg2), lwd.arcs = E(gg2)$weight/3,  cex.labels = 0.75,pch.nodes = 21, lwd.nodes = 2, line = -0.6)
dev.off()



#########################################################################
# Commented out useful commands:

# Reduce duplicate entries to one entry, but count the number of occurences
#mails.counted <- ddply(mails, .(From, To), summarise, count = length(To))
#mails.counted$From <- sub("@enron.com", "", mails.counted$From)
#mails.counted$To <- sub("@enron.com", "", mails.counted$To)
#mails.sender <- unique(mails.counted$From)
# we want only mails to people within the data
#mails.connected <- subset(mails.counted, To %in% mails.sender)

#top.ten <- mails.connected[order(mails.connected$count, decreasing=TRUE),][1:10,]
#top.twenty <- mails.connected[order(mails.connected$count, decreasing=TRUE),][1:20,]
#more.than.fifty <- subset(mails.connected, count > 100)
# !is.na(match(mails.counted$To, mails.sender))
#arcplot(as.matrix(mails.counted)[1:17,1:2])
#arcplot(as.matrix(mails.connected)[,1:2], lwd.arcs = log(mails.connected$count)+1)
#arcplot(as.matrix(top.ten[,1:2]), cex.labels = 0.5,lwd.arcs = log(top.ten$count)+1, pch.nodes = 21, lwd.nodes = 2, line = 0, cex.nodes = top.ten$count*0.001)

