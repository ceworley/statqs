library("exams")
set.seed(666)

# folders = list.dirs(recursive = F)

name3 = "p2_exercises"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))

# s = c("p1g_continuous_distributions/estimate_area.Rmd",
#       "p1g_continuous_distributions/normal_basic.Rmd",
#       "p1g_continuous_distributions/standard_score.Rmd")

s = gsub(".Rmd","",s)

myexam <- c(paste(s,".Rmd",sep=""))

exams2moodle(myexam, n = 50,name=name3,
             stitle=s,
             dir = "out",
             converter="pandoc-mathml"
             )

tx  <- readLines(paste0("out/",name3,".xml",collapse=""))
tx2  <- gsub(pattern = "𝐱", replace = "x", x = tx)
tx2  <- gsub(pattern = "𝐰", replace = "w", x = tx2)
tx2  <- gsub(pattern = "𝙰𝙽𝙳", replace = "AND", x = tx2)
tx2  <- gsub(pattern = "𝙾𝚁", replace = "OR", x = tx2)
tx2  <- gsub(pattern = "𝙽𝙾𝚃", replace = "NOT", x = tx2)
writeLines(tx2, con=paste0("out/",name3,"_fixed.xml",collapse=""))

