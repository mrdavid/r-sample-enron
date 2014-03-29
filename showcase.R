setwd("/Users/davidr/enron/")

# Function to read a single email message's headers
# Adapted from 'Machine Learning for Hackers' (Conway, White)
get.message.headers  <- function(path){
  con <- file(path, open="rt", encoding="ascii")
  text <- readLines(con)
  # headers end at the first empty line
  headers <- text[seq(1,which(text=="")[1]-1, 1)]
  close(con)
  return(headers)
}

parse.message <- function(headers){
  from <- grep("^from:",headers,ignore.case=TRUE,value=TRUE)[1]
  from <- sub("^From:\\s*", "", from)
  
  to <- grep("^to:",headers,ignore.case=TRUE,value=TRUE)[1]
  to <- sub("^to:\\s*", "", to)
  # TODO: Ignore mails sent to several senders for the moment
  if(regexpr(",",to,fixed=TRUE)[1] != -1){
    to <- NA
  }
  return(to)
}

# Insert a row into an existing data frame
# from http://stackoverflow.com/questions/11561856/add-new-row-to-dataframe
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}


# message 11 contains message with several receipients 
print(parse.message(get.message.headers("enron_mail_20110402/maildir/skilling-j/sent/12.")))