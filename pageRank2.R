initPageRank<-function(){
      setwd("~/Cours/Coursera/Mining Massive Datasets/googleGraph")
      library(data.table)
      library(dplyr)    
      data<<-data.table(read.csv("web-Google.txt",header = F,skip=4,sep="\t",col.names=c("from","to")))
}

readData<-function(file){
      #"web-Google.txt"
      data.table(read.csv(file,header = F,skip=4,sep="\t",col.names=c("from","to")))
}
nNode<-function(dataT){
      uniqueF<-distinct(select(dataT,from))
      uniqueT<-distinct(select(dataT,to))
      setnames(uniqueT,"to","from")
      nrow(distinct(rbind(uniqueF,uniqueT)))
}

reID<-function(dataT){
      
      uniqueF<-distinct(select(dataT,from))
      uniqueT<-distinct(select(dataT,to))
      setnames(uniqueT,"to","from")
      dist<-distinct(rbind(uniqueF,uniqueT))
      IDmap<-cbind(dist,seq(nrow(dist)))
      setnames(IDmap,"from","oldID")
      setnames(IDmap,"V2","newID")
      IDmap
}

pageRank<-function(dataT,epsil=0.0001){
      #map les ID
      IDmap <- reID(dataT)
      setkey(IDmap,oldID)
      
      #function de recup des ID
      getnewID<-function(oldI){
            IDmap[oldID==oldI,newID]
      }
      getoldID<-function(newI){
            IDmap[newID==newI,oldID]
      }
      
      #ajoute le outDegree au data
      nNodes<-nNode(dataT)
      dataT[ , `:=`(outD = .N) , by = from]
      dataT<-mutate(dataT,preri = 1/outD)
      
      #Init rank1 et rank2
      R1<-rep(1/nNodes,nNodes)
      R2<-rep(0,nNodes)
      rank<-data.table(R1,R2)

      oddIt<-2
     # setkey(dataT,)
      while (distV(rank[,R1],rank[,R2])>epsil){
            #Iteration 2
            ranki<-rank[[getnewID(dataT[1,from]),3-oddIt]]
            outDi<-dataT[1,preri]
            oldIDi<- dataT[1,from]
            for (rowNum in seq(nrow(dataT))) {  
                  
                  if(dataT[rowNum,from] == oldIDi){
                         set(dataT,rowNum,4L,outDi*ranki)
                       # set(dataT,rowNum,4L,dataT[rowNum,preri]*rank[[getnewID(dataT[rowNum,from]),3-oddIt]])
                       # dataT[rowNum,preri:= dataT[rowNum,preri]*rank[getnewID(dataT[rowNum,from]),3-oddIt]]
                  }
                  else{
                        oldIDi<- dataT[rowNum,from]
                        ranki<-rank[[getnewID(oldIDi),3-oddIt]]
                        outDi<-dataT[rowNum,preri]
                        set(dataT,rowNum,4L,outDi*ranki)
                  }
            }
            dataT<-aggregate(. ~ to, data=select(dataT,to,preri), FUN = sum)
            
            #dataT<-select(dataT,to,preri)[, lapply(.SD, sum), by = preri]
            
#             #Iteration calcul de r'
#             for (j in 1:nNodes){
#                   oldj<-getoldID(j)
#                   rankj<-0
#                   toJ<-dataT[to==oldj,]
#                   if(nrow(toJ)!=0){
#                         for (k in seq(nrow(toJ))){
#                               ri<-rank[getnewID(toJ[k,from]),3-oddIt]
#                               di<-toJ[k,outD]
#                               rankj<- rankj + 0.8*ri/di
#                         }
#                   }
#                   else rankj<-0
#                   rank[j,oddIt]<-rankj
#             }
            
            #soustraction de S a r' 
            S<-0
            for (i in seq(nNodes)){
                  S<-S+ rank[[i,oddIt]]
            }
            for (i in seq(nNodes)){
                  rank[i,oddIt]<-rank[[i,oddIt]] +(1-S)/nNodes
            }
            oddIt<-3-oddIt
      }
      cbind(rank[[,3-oddIt]],IDmap[,oldID])

      
}

distV<-function(vect1, vect2){
      res<-0
      for (i in 1:length(vect1)){
            res<- res+ abs(vect1[i]-vect2[i])
      }
      res
      
}