
dirlist<-dir()

if(length(grep("conn",dirlist))>0){
rmlist<-dirlist[grep("conn",dirlist)]

for(i in 1:length(rmlist)){
	
	system(paste0("rm -rf ",rmlist[i],"/*"))
	file.remove(rmlist[i])

}
}

