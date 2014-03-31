#!/bin/bash

## download and untar the Enron email corpus
#wget http://www.cs.cmu.edu/~enron/enron_mail_20110402.tgz
#tar xvfz enron_mail_20110402.tgz

## create a list of 'sent' folders
echo "Creating list of 'sent' folders. This may take a moment."
find . -type d  | grep -i sent  | grep -v presentation > sent_folders

## create an R script with the right working directory (the current dir)
echo -n 'setwd("' > run.showcase.R
pwd | tr -d '\r\n'>> run.showcase.R
echo '")' >> run.showcase.R
cat showcase.R | grep -v setwd >> run.showcase.R

## run!
echo "Running R script."
R -f run.showcase.R
