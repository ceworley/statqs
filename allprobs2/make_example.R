library("exams")
set.seed(100)

name = "allproblems"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))


# for(prob in s){
#   bits = unlist(strsplit(prob,"/"))
#   n = length(bits)
#   dir = paste0(bits[1:(n-1)],collapse="/")
#   myexam <- c(paste0(prob,".Rmd",sep=""))
#   mydir = paste0("out/",dir,collapse="")
#   dir.create(mydir,recursive=T,showWarnings=F)
#   exams2html(myexam,n=3,seed=1:3,name=bits[n],
#              dir = mydir,solution=F
#   )
#   exams2html(myexam,n=3,seed=1:3,name=paste0(bits[n],"_sol"),
#              dir = mydir,question=F,solution=T
#   )
# }

html1 = '<!DOCTYPE html>
<html>
<head>
<title>All Problems</title>
<style>
/* Remove default bullets */
ul, #myUL {
  list-style-type: none;
}
/* Remove margins and padding from the parent ul */
#myUL {
  margin: 0;
  padding: 0;
}
/* Style the caret/arrow */
.caret {
  cursor: pointer;
  user-select: none; /* Prevent text selection */
}
/* Create the caret/arrow with a unicode, and style it */
.caret::before {
  content: "\25B6";
  color: black;
  display: inline-block;
  margin-right: 6px;
}
/* Rotate the caret/arrow icon when clicked on (using JavaScript) */
.caret-down::before {
  transform: rotate(90deg);
}
/* Hide the nested list */
.nested {
  display: none;
}
/* Show the nested list when the user clicks on the caret/arrow (with JavaScript) */
.active {
  display: block;
}
</style>
</head>
<body>
<script>
var toggler = document.getElementsByClassName("caret");
var i;

for (i = 0; i < toggler.length; i++) {
  toggler[i].addEventListener("click", function() {
    this.parentElement.querySelector(".nested").classList.toggle("active");
    this.classList.toggle("caret-down");
  });
}
</script>'

html2 = '</body>
</html>'


mys = '<ul id="myUL">\n'
folders = character()
lasti = 0
for(prob in s){
  bits = unlist(strsplit(prob,"/"))
  for(i in 1:length(bits)){
    fol = paste0(bits[1:i],collapse="")
    fol2 = substr(bits[i],4,1000)
    if(!(fol %in% folders)){
      if(lasti>i){
        reps = lasti-i
        news = paste0(rep("</ul></li>\n",reps),collapse="")
        mys = paste0(mys,news,collapse="")
      }
      if(i<length(bits)){
        news = paste0(paste0(rep(' ',i),collapse=""),'<li><span class="caret">',fol2,'</span>\n<ul class="nested">\n',
                   collapse="")
        mys = paste0(mys,news,collapse="")
        lasti = i
      } else {
        news = paste0(paste0(rep(' ',i),collapse=""),'<li>',fol2,"</li>\n",collapse="")
        mys = paste0(mys,news,collapse="")
        lasti = i
      }
      folders=c(folders,fol)
    }
  }
}
news = paste0(rep("</ul></li>\n",lasti-1),collapse="")
mys = paste0(mys,news,'</ul>',collapse="")

write.table(paste0(html1,mys,html2,collapse="\n"),"toc.html",col.names = )




