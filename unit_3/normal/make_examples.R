library("exams")
set.seed(145)


name3 = "p3_normal_examples"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 1,name=name3,
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

