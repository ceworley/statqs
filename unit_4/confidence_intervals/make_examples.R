library("exams")
set.seed(123)

name = "p4_confint_examples"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 1,name=name,
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

