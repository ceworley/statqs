library("exams")

folder = "03_central_limit_theorem/02_normal/"
n = 50
outdir = "outmoodle"
files = list.files(folder,pattern=".Rmd")
name = "03_02_normal"
fn2 = paste0(outdir,"/",name,".xml")
exams2moodle(paste0(folder,files),n=n,dir=outdir,converter="pandoc-mathml",
             name=name)
tx  <- readLines(fn2)
tx  <- gsub(pattern = "𝐱", replace = "x", x = tx)
tx  <- gsub(pattern = "𝐰", replace = "w", x = tx)
tx  <- gsub(pattern = "𝙰𝙽𝙳", replace = "AND", x = tx)
tx  <- gsub(pattern = "𝙽𝙾𝚃", replace = "NOT", x = tx)
tx  <- gsub(pattern = "𝙾𝚁", replace = "OR", x = tx)
writeLines(tx, con=fn2)



