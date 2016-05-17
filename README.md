# pollutantmean
R file as exercise to week 2 homework assignment for JHU R Programming coursera course

##works for id<-vector length 1

     pollutantmean<-function(directory,pollutant="sulfate",id=1:332){
         setwd(directory)
         sitefilename<-function(id){
                 if (id<10) {
                 paste(as.character(0),as.character(0),as.character(id),".csv",sep="")
         } else if (id<100) {
                 paste(as.character(0),as.character(id),".csv",sep="")
         } else paste(as.character(id),".csv",sep="")
         }
     
         mydata<-read.csv(sitefilename(id))
        
         polmean<-function(pollutant){
                 if (pollutant=="sulfate"){
                         mean(mydata[,2],na.rm=TRUE)
                 } else if (pollutant=="nitrate"){
                         mean(mydata[,3],na.rm=TRUE)
                 } else print(NA)
         }
         polmean(pollutant)
    }
