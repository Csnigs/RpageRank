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

pageRank<-function(dataT){
      #map les ID
      IDmap <- reID(dataT)
      
      #function de recup des ID
      getnewID<-function(oldI){
            IDmap[oldID==as.character(oldI),newID]
      }
      getoldID<-function(newI){
            IDmap[newID==as.character(newI),oldID]
      }
      
      #ajoute le outDegree au data
      nNodes<-nNode(dataT)
      dataT[ , `:=`(outD = .N) , by = from]
      
      #Init rank1 et rank2
      rank1<-rep(1/nNodes,nNodes)
      rank2<-rep(0,nNodes)
      
      #Iteration calcul de r'
      for (j in 1:nNodes){
            oldj<-getoldID(j)
            rankj<-0
            toJ<-dataT[to==oldj,]
            if(nrow(toJ)!=0){
                  for (k in seq(nrow(toJ))){
                        ri<-rank1[getnewID(toJ[k,from])]
                        di<-toJ[k,outD]
                        rankj<- rankj + 0.8*ri/di
                  }
            }
            else rankj<-0
            rank2[j]<-rankj
      }
      
      #soustraction de S a r' 
      S<-0
      for (i in seq(nNodes)){
            S<-S+ rank2[i]
      }
      for (i in seq(nNodes)){
            rank2[i]<-rank2[i] +(1-S)/nNodes
      }
      
      rank2
      #data[order(to)]
      #dataT[to==1,]
      
}