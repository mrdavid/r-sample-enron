# Requires library "plyr"
# Requires 'arcdiagram' which needs to be downloaded from github:
#   library(devtools)
#   install_github("arcdiagram", username = "gastonstat")
# (You also need the library "devtools" for this)
library(plyr)
library(arcdiagram)
require(igraph)

setwd("/Users/davidr/enron/")

# Function to read a single email message's headers
# Adapted from 'Machine Learning for Hackers' (Conway, White)
get.message.headers  <- function(path){
  tryCatch({
    con <- file(path, open="rt", encoding="ascii")
    text <- readLines(con)
    # headers end at the first empty line
    headers <- text[seq(1,which(text=="")[1]-1, 1)]
    close(con)
    return(headers)
  },
  error = function(e){
    return(NA)
  })
}

parse.message <- function(headers){
  if(is.na(headers)){
    return(c(NA, NA))
  }
  
  from <- grep("^from:",headers,ignore.case=TRUE,value=TRUE)[1]
  from <- sub("^From:\\s*", "", from, ignore.case=TRUE)
  
  to <- grep("^to:",headers,ignore.case=TRUE,value=TRUE)[1]
  to <- sub("^to:\\s*", "", to, ignore.case=TRUE)
  # TODO: Ignore mails sent to several senders for the moment
  if(!is.na(to)){
    if(regexpr(",",to,fixed=TRUE)[1] != -1){
      to <- NA
    }
  }
  return(c(from, to))
}

# message 11 contains message with several receipients
mail_folders <- file("sent_folders", open="rt", encoding="ascii")
mail_files_paths <- readLines(mail_folders)
close(mail_folders)
mail_files <- unlist(lapply(mail_files_paths, function(path){
  paste(path, dir(path), sep="/")
}))
mails <- sapply(mail_files, function(filename){ 
  parse.message(get.message.headers(filename))
})
# turn into data frame, removing cases that could not be parsed using na.omit
mails <- na.omit(data.frame(mails[1,], mails[2,]))
colnames(mails) <- c("From", "To")

mails.sender <- unique(mails$From)
mails.connected <- subset(mails, To %in% mails.sender)
mails.counted <- ddply(mails.connected, .(From, To), summarise, weight = length(To))
g <- graph.data.frame(mails.counted, directed=FALSE)
d <- sort(degree(g),decreasing=TRUE)
gg <- delete.edges(g, which(E(g)$weight < 150))
gg <- delete.vertices(gg, which(degree(gg) < 1))

pdf("out.pdf")
arcplot(get.edgelist(gg), lwd.arcs = E(gg)$weight/100, cex.labels = 0.45,pch.nodes = 21, lwd.nodes = 2, line = 0, cex.nodes = degree(gg)*0.5)
dev.off()
#gg <- graph.data.frame(subset(mails.connected, From %in% names(top.twenty) && To %in% names(top.twenty)), directed=FALSE)

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