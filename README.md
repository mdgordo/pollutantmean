# pollutantmean
R file as exercise to week 2 homework assignment for JHU R Programming coursera course

## only works if you setwd to parent first
      setwd("..")
      setwd("~/Desktop/coursera/")

      pollutantmean<-function(directory,pollutant="sulfate",id=1:332){
        setwd(directory)
        sitefilenames<-function(id){
                ifelse(id<10,paste(as.character(0),as.character(0),as.character(id),".csv",
                          sep=""),ifelse(id<100,paste(as.character(0),as.character(id),".csv"
                                        ,sep=""),paste(as.character(id),".csv",sep="")))
        }
        
        allfiles<-sitefilenames(id[-1])
        basefile<-sitefilenames(id[1])
        mydata<-read.csv(basefile)
        if(length(allfiles)>1){
                for(i in allfiles){
                        mydata<-rbind(mydata,read.csv(i))
                }     
        }
       
        polmean<-function(pollutant){
                if (pollutant=="sulfate"){
                        mean(mydata[,2],na.rm=TRUE)
                } else if (pollutant=="nitrate"){
                        mean(mydata[,3],na.rm=TRUE)
                } else print(NA)
        }
        polmean(pollutant)
      }

