library("exams")
set.seed(100)

name = "allproblems"
s = gsub(".Rmd","",list.files(pattern=".Rmd",recursive = T))

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
  content: "\\25B6";
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
#myBtn1 {
  display: block;
  position: fixed;
  bottom: 20px;
  right: 300px;
  z-index: 99;
  font-size: 18px;
  border: none;
  outline: none;
  background-color: black;
  color: white;
  cursor: pointer;
  padding: 15px;
  border-radius: 4px;
}
#myBtn2 {
  display: block;
  position: fixed;
  bottom: 20px;
  right: 130px;
  z-index: 99;
  font-size: 18px;
  border: none;
  outline: none;
  background-color: blue;
  color: white;
  cursor: pointer;
  padding: 15px;
  border-radius: 4px;
}
#myBtn3 {
  display: block;
  position: fixed;
  bottom: 20px;
  right: 30px;
  z-index: 99;
  font-size: 18px;
  border: none;
  outline: none;
  background-color: red;
  color: white;
  cursor: pointer;
  padding: 15px;
  border-radius: 4px;
}

#myBtn1:hover {
  background-color: #555;
}

</style>
</head>
<body>
%s
<button onclick="topFunction()" id="myBtn1" title="Go to top">Top</button>
<button onclick="qFunction()" id="myBtn2" title="Go to question (shortcut=`q`)">Question</button>
<button onclick="aFunction()" id="myBtn3" title="Go to answer (shortcut=`a`)">Answer</button>
<center>

<iframe src="%s" frameborder=0 name="cake" id="cake" style ="border:2px solid black;height:95vh;background-color:#eef1f1;" width="95%%"></></iframe>
</center>
<center>
<iframe src="%s" frameborder=0 name="duck" id="duck" style ="border:2px solid black;height:95vh;background-color:#ffdada;" width="95%%"></></iframe>
</center>
<script>
var toggler = document.getElementsByClassName("caret");
var i;
for (i = 0; i < toggler.length; i++) {
  toggler[i].addEventListener("click", function() {
    this.parentElement.querySelector(".nested").classList.toggle("active");
    this.classList.toggle("caret-down");
  });
}
function changeUrl(site,site2) {
    document.getElementsByName("cake")[0].src = site;
    document.getElementsByName("duck")[0].src = site2;
}
function topFunction() {
  document.body.scrollTop = 0;
  document.documentElement.scrollTop = 0;
}
function qFunction() {
  document.getElementsByName("cake")[0].scrollIntoView();
}
function aFunction() {
  document.getElementsByName("duck")[0].scrollIntoView();
}
document.addEventListener("keypress", function onEvent(event) {
    if (event.key === "q") {
        document.getElementsByName("cake")[0].scrollIntoView();
    }
    if (event.key === "t") {
        document.body.scrollTop = 0;
    }
    if (event.key === "a") {
        document.getElementsByName("duck")[0].scrollIntoView();
    }

});
</script>
</body>
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
        news = paste0(paste0(rep(' ',i),collapse=""),'<li><button onclick="changeUrl(\'',
                      paste0("out/",prob,"1.html",collapse=""),
                      '\',\'',
                      paste0("out/",prob,"_sol1.html",collapse=""),
                      '\')">',fol2,'</button><button onclick="changeUrl(\'',
                      paste0("out/",prob,"2.html",collapse=""),
                      '\',\'',
                      paste0("out/",prob,"_sol2.html",collapse=""),
                      '\')">V2</button><button onclick="changeUrl(\'',
                      paste0("out/",prob,"3.html",collapse=""),
                      '\',\'',
                      paste0("out/",prob,"_sol3.html",collapse=""),
                      '\')">V3</button> </li>\n',collapse="")
        mys = paste0(mys,news,collapse="")
        lasti = i
      }
      folders=c(folders,fol)
    }
  }
}
news = paste0(rep("</ul></li>\n",lasti-1),collapse="")
mys = paste0(mys,news,'</ul>',collapse="")


p1 = paste0("out/",s[1],"1.html",collapse="")
p2 = paste0("out/",s[1],"_sol1.html",collapse="")
page = sprintf(html1,mys,p1,p2)

write.table(page,"toc.html",quote=F,col.names=F,qmethod="d",row.names=F)



for(prob in s){
  bits = unlist(strsplit(prob,"/"))
  n = length(bits)
  dir = paste0(bits[1:(n-1)],collapse="/")
  myexam <- c(paste0(prob,".Rmd",sep=""))
  mydir = paste0("out/",dir,collapse="")
  dir.create(mydir,recursive=T,showWarnings=F)
  qname = paste0(mydir,"/",bits[n],1,".html",collapse="")
  if(!file.exists(qname)){
    exams2html(myexam,n=3,seed=1:3,name=bits[n],template="plain8B.html",mathjax=T,
               dir = mydir,solution=F,question=prob
    )
    exams2html(myexam,n=3,seed=1:3,name=paste0(bits[n],"_sol"),template="plain8B.html",mathjax=T,
               dir = mydir,question=F,solution=paste0("solution_",prob,collapse="")
    )
    for(i in 1:3){
      qname = paste0(mydir,"/",bits[n],i,".html",collapse="")
      print(qname)
      myf = paste(readLines(qname),collapse="\n")
      rep1 = sprintf("<ol>\n<li>\n%s/%s",dir,bits[n])
      rep2 = sprintf("<small><i>%s/%s_v%s</i></small>",dir,bits[n],i)
      myf = sub(rep1,rep2,myf)
      rep1 = "</li>\n</ol>\n\n</body>"
      rep2 = "\n\n</body>"
      myf = sub(rep1,rep2,myf)
      write.table(myf,qname,row.names=F,col.names=F,qmethod="d",quote=F)
      ###
      qname = paste0(mydir,"/",bits[n],"_sol",i,".html",collapse="")
      myf = paste(readLines(qname),collapse="\n")
      rep1 = sprintf("<ol>\n<li>\nsolution_%s/%s",dir,bits[n])
      rep2 = sprintf("<small><i>solution_%s/%s_v%s</i></small>",dir,bits[n],i)
      myf = sub(rep1,rep2,myf)
      rep1 = "</li>\n</ol>\n\n</body>"
      rep2 = "\n\n</body>"
      myf = sub(rep1,rep2,myf)
      write.table(myf,qname,row.names=F,col.names=F,qmethod="d",quote=F)
    }
  }
}
