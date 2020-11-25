library("exams")
set.seed(765)

name3 = "p3_exam_examples"
s = gsub(".Rmd","",list.files("questions/",pattern=".Rmd",recursive = T))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n=1, name=name3,
             stitle=s,
             edir = "questions",
             dir = "out",
             converter="pandoc-mathml"
             )

