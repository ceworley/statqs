library("exams")
set.seed(951)

# folders = list.dirs(recursive = F)

name3 = "standard_normal_boundaries"

myexam <- list("z_from_left_area.Rmd",
               "z_from_right_area.Rmd",
               "z_from_central_area.Rmd",
               "z_from_twotail_area.Rmd")

s = c("z_from_left_area",
      "z_from_right_area",
      "z_from_central_area",
      "z_from_twotail_area")

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

