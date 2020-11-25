library("exams")
set.seed(23456)

s = c("check_interval_of_typical_measurements",
      "compare_SD_same_shape",
      "compare_SD_same_width",
      "estimate_SD",
      "get_interval_of_typical_measurements",
      "get_IQR",
      "get_MAD",
      "get_range",
      "get_SD",
      "get_SD2",
      "get_SD_prop",
      "get_VAR",
      "get_VAR2",
      "get_VAR_prop")

s = c("get_VAR_prop",
      "get_SD_prop")

s = ("check_interval_of_typical_measurements")

myexam <- c(paste(s,".Rmd",sep=""))


name3 = "spread"
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

