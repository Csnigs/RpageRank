initPageRank<-function(){
      setwd("~/Cours/Coursera/Mining Massive Datasets/googleGraph")
      library(data.table)
      library(dplyr)    
      dataP<<-data.table(read.csv("web-Google.txt",header = F,skip=4,sep="\t",col.names=c("from","to")))
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

pageRank<-function(dataP,epsil=0.1){
      dataT<-copy(dataP)
      iteration<-0
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
      #dataT<-mutate(dataT,preri = 0,R1 = 1/nNodes,R2 =0,newIDfrom = getnewID(from),newIDto = getnewID(to))
      #Init rank1 et rank2
      rank<-copy(IDmap)
      prevrank<-copy(IDmap)
      #rank<-mutate(rank,Rank= 1/nNodes)
      rank[,Rank:=1/nNodes]
      prevrank[,Rank:=0]
      dist<-distV(rank[,Rank],prevrank[,Rank])
      print(dist)
     # setkey(dataT,)
      while (dist>epsil){

            setkey(dataT,from)
            dataT<-rank[dataT,nomatch=NA]
            setnames(dataT,"oldID","from")
            dataT[,newID:=NULL]
            dataT[ , `:=`(preri = Rank*0.8/outD)]
            agregPreri<-data.table(group_by(dataT,to) %>%summarise( agRi = sum(preri)))
            dataT[,Rank:=NULL]
            prevrank<-rank          
            
            setnames(agregPreri,"to","oldID")
            setkey(agregPreri,oldID)
            setkey(IDmap,oldID)
            rank<-agregPreri[IDmap,nomatch=NA]
            set(rank,which(is.na(rank[["agRi"]])),"agRi",0)
            S<-(1 - sum(agregPreri[,agRi]))/nNodes
            setnames(rank,"agRi","Rank")
            rank[,Rank:=Rank +S]
            drank<-data.table(rank[,Rank],prevrank[,Rank])
            drank[,dist:=abs(V1-V2)]
            dist<-sum(drank[,dist])
            iteration<-iteration+1
            print(iteration)
            print(dist)
            remove(list=c("agregPreri","drank"))
            
#             dataT[ , `:=`(preri = R1/outD)]
#             agregPreri<-group_by(dataT,to) %>%summarise( agRi = sum(preri))
#             dataT<- agregPreri[dataT,nomatch=NA]
#             dataT[,R2:=NULL]
#             setnames(dataT,"R1","R2")
#             setnames(dataT,"agRi","R1")
# 
#             #soustraction de S a r' 
#             S<-(1 - sum(agregPreri[,agRi]))/nNodes
#             dataT[ , `:=`(R1 = R1 +S)]
      }

      rank

      
}

distV<-function(vect1, vect2){
      res<-0
      for (i in 1:length(vect1)){
            res<- res+ abs(vect1[i]-vect2[i])
      }
      res
      
}