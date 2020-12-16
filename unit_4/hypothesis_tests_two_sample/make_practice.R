library("exams")
set.seed(3333)

name = "p4_ht_2samp_practice"
s = gsub(".Rmd","",list.files("questions",pattern=".Rmd"))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 50,name=name,
             edir="questions",
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

