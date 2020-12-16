library("exams")
set.seed(685)

name = "p4_exam4_real"
s = gsub(".Rmd","",list.files("questions",pattern=".Rmd"))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n=50, name=name,
             edir="questions",
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

