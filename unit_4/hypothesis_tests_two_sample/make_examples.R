library("exams")
set.seed(8462)

NxME = "p4_ht_2samp_examples"
s = gsub(".Rmd","",list.files("questions",pattern=".Rmd"))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 1,name=NxME,
             edir="questions",
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

