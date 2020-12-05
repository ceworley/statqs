library("exams")
set.seed(123)

name = "p4_ht_1samp_examples"
s = gsub(".Rmd","",list.files("questions",pattern=".Rmd"))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 1,name=name,
             edir="questions",
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

