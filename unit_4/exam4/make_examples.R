library("exams")

NxME = "p4_exam4_examples"
s = gsub(".Rmd","",list.files("questions",pattern=".Rmd"))

myexam <- c(paste(s,".Rmd",sep=""))

set.seed(573)
exams2moodle(myexam, n = 1,name=NxME,
             edir="questions",
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

