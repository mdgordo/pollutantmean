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

## count number of complete cases in file, return data frame with id in column one and complete cases in column 2

       complete<-function(directory,id=1:332){
        setwd(directory)
        sitefilenames<-function(id){
                ifelse(id<10,paste(as.character(0),as.character(0),as.character(id),".csv",
                                   sep=""),ifelse(id<100,paste(as.character(0),as.character(id),".csv"
                                                               ,sep=""),paste(as.character(id),".csv",sep="")))
        }
        basefile<-sitefilenames(id[1])
        basecomplete<-sum(complete.cases(read.csv(basefile)))
        baseframe<-matrix(c(id[1],basecomplete),1,2)
        for(i in id[-1]){
                allfiles<-sitefilenames(i)
                allcomplete<-sum(complete.cases(read.csv(allfiles)))
                baseframe<-rbind(baseframe,c(i,allcomplete))
        }     
        colnames(baseframe)<-c("id","nobs")
        print(as.data.frame(baseframe))
       }

## correlation between sulfate and nitrate pollution for all files that meet a certain threshold of complete cases

       corr<-function(directory, threshold=0){
        cc<-complete(directory)
        thresh<-cc[cc$nobs>threshold,]
        id<-thresh$id
        if(length(id)==0){
                print(0)} else{
        sitefilenames<-function(id){
                ifelse(id<10,paste(as.character(0),as.character(0),as.character(id),".csv",
                                   sep=""),ifelse(id<100,paste(as.character(0),as.character(id),".csv"
                                                               ,sep=""),paste(as.character(id),".csv",sep="")))
        }
        
        basefile<-sitefilenames(id[1])
        basedata<-read.csv(basefile)
        corr<-cor(basedata[,2],basedata[,3],"complete.obs")
        for(i in id[-1]){
                allfiles<-sitefilenames(i)
                alldata<-read.csv(allfiles)
                allcorr<-cor(alldata[,2],alldata[,3],"na.or.complete")
                corr<-c(corr,allcorr)
        }
        print(corr)
        }
       }
