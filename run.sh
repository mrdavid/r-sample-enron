wget http://www.cs.cmu.edu/~enron/enron_mail_20110402.tgz
tar xvfz enron_mail_20110402.tgz
find . -type d  | grep -i sent > sent_folders
R -f showcase.R
