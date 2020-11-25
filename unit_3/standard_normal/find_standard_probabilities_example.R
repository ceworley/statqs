library("exams")
set.seed(678)

# folders = list.dirs(recursive = F)

name3 = "find_standard_probabilities_example"

myexam <- list("leftarea_from_z.Rmd",
               "rightarea_from_z.Rmd",
               "area_between_two_zs.Rmd",
               "centralarea_from_z.Rmd",
               "twotailarea_from_z.Rmd")

s = c("leftarea_from_z",
      "rightarea_from_z",
      "area_between_two_zs",
      "centralarea_from_z",
      "twotailarea_from_z")

exams2moodle(myexam, n = 1,name=name3,
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

