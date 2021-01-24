create.cluster.plot.custom = function(data,add.labels,cols,clusterno=NULL,gower=TRUE,create.plot=TRUE){

   require(ggrepel)
   require(cluster)
   require(stringdist)
   require(factoextra)
   require(dplyr)
   require(tidyverse)
   require(NbClust)



   data = data[ ,c(cols,"fitness")]

   
   # print("Converting to dissimilarity matrix...")
   if (gower == TRUE){
      data[cols] <- lapply(data[cols], factor)  ## as.factor() could also be used
      daisy.mat <- as.matrix(daisy(data[cols], metric="gower"))
   }else{   

   df = data %>% unite(lang, cols,sep="",remove=FALSE)
   daisy.mat <- 1-stringsimmatrix(df$lang,df$lang,useNames="strings",method="lv") #()
   }   # saveMDS(daisy.mat)
   
   # print("Beginning MDS...")

   # mds.data = loadMDS()


   mds.data = cmdscale(daisy.mat)
   daisy.mat <- mds.data


   colnames(mds.data) = c("x","y")

   # print("Conducting clustering...")

   if (is.null(clusterno)){

      vec <- c()
      methods = c("silhouette","wss","gap_stat")
      # print(length(unique(daisy.mat)))
      for ( m in 1:length(methods)){
         n_clust<-fviz_nbclust(daisy.mat, kmeans, method = methods[m],k.max = 20,verbose=FALSE,nstart = 5,nboot=50)

         n_clust<-n_clust$data
         vec[m]<-as.numeric(n_clust$clusters[which.max(n_clust$y)])
      }

      clusterno = median(vec)
   }
   
   res <- kmeans(daisy.mat,clusterno)
   # hc <- hclust(as.dist(daisy.mat), method="single")
   # res <- as.data.frame(cutree(hc, k = clusterno))$res

   data = cbind(data,mds.data)
   data$cluster=factor(res$cluster)

   centers = as.data.frame(res$centers)

   centers$cluster = factor(seq.int(nrow(res$centers)))

   labels = data.frame()
   df.stats = data.frame()


   # print("Plotting data...")

   for (i in 1:nrow(res$centers)){
   df <- read.csv(text=paste(cols,collapse=","),header=TRUE)
   row <- c()
   str <- ""
   for (term in c(1:length(cols))){
         term_list <- sort(summary(as.factor(data[res$cluster==i,term])),decreasing=TRUE)

         if (names(term_list[1]) == "(Other)"){
         vec <- term_list[2]

         } else{
         vec <- term_list[1]

         }

         row[term]=vec[[1]]/table(res$cluster)[[i]]
         str = paste(str," ",as.factor(colnames(data[1,])[term]),": ",word=names(vec)," - ",freq=round(vec[[1]]/table(res$cluster)[[i]]*100,2),"%",sep="","\n")
      }

      proportion.pop = table(res$cluster)[[i]]/length(data[,1])
      fitness.mean = mean(data[res$cluster==i,"fitness"])
      fitness.sd = sd(data[res$cluster==i,"fitness"])
      df[nrow(df)+1,] = row
      df = cbind(df,prop=proportion.pop,mean=fitness.mean,sd=fitness.sd)
      df.stats <- rbind(df,df.stats)

   str = paste(str,"Proportion: ",round(table(res$cluster)[[i]]/length(data[,1]),3),"\nMean Fitness: ",format(round(mean(data[res$cluster==i,"fitness"]), 5)),
   "\nsd: ",format(round(sd(data[res$cluster==i,"fitness"]), 5)))

   labels = rbind(labels,data.frame(label=str,cluster=i))   

   }

   result = list()

   if (create.plot){

      g <- ggplot(data, aes(x,y,color = cluster, fill = cluster)) +
      geom_point(size=0.4)  +
         stat_ellipse(type = "t",geom = "polygon",alpha = 0.1) 
            
      centers = cbind(aggregate(data=data, x ~ cluster,mean),y=aggregate(data=data, y ~ cluster,mean)$y)

      if (add.labels==TRUE){
         g <- g + geom_text(data = merge(centers,labels), aes(x,y,color=cluster,label = label),color="black")
      }


      result[["plot"]] = g
   }
   result[["df"]] = df.stats
   return(result)

}

create.elbow.plot = function(data,k.start,k.max,cols){

   df = data %>% unite(lang, cols,sep="-",remove=FALSE)
   daisy.mat <- stringsimmatrix(df$lang,df$lang,useNames="strings",method="lv")
   mds.data = cmdscale(daisy.mat)
   colnames(mds.data) = c("x","y")

   var <- c()
   for (k in k.start:k.max ){
   var <- c(var,kmeans(mds.data, k)$tot.withinss)

   }

   return(data.frame(clusters=k.start:k.max,var=var))
}


library(jsonlite)
agents = invisible(stream_in(file(paste('./wordlists/ABC/wordList_25.json',sep="")),verbose=FALSE))
agents$hashcode = !is.na(agents$hashcode)


data=subset(agents,hashcode==TRUE&resources==1000&environment==4&trial==7)
data.rand=subset(agents,hashcode==FALSE &resources==2000&environment==6)

cols=c("A","B","C")
require(tidyverse)
require(stringdist)
df = data %>% unite(lang, cols,sep="",remove=FALSE)
daisy.mat <- 1-stringsimmatrix(df$lang,df$lang,useNames="strings",method="lv")
hc <- hclust(as.dist(daisy.mat),method="complete")
plot(hc)

head(df)
mds.data = cmdscale(daisy.mat)
colnames(mds.data) = c("x","y")
head(mds.data)
mds.data
library(ggrepel)
kmeans(daisy.mat,5)$centers
require(factoextra)
n_clust<-fviz_nbclust(daisy.mat, kmeans, method = "silhouette",k.max = 6)
n_clust
summary(n_clust)
as.numeric(n_clust$clusters[which.max(n_clust$y)])

ggplot(data=as.data.frame(mds.data),aes(x=x,y=y,colour=factor(kmeans(daisy.mat,5)$centers))) + geom_point()

ggplot(data=as.data.frame(mds.data),aes(x=x,y=y,colour=factor(df$cluster))) + geom_point()

par(cex=0.5,font=3)
plot(hc,hang=-1)
rect.hclust(hc, k = 10, border = "red")


plot(hc)
cutreeHybrid(hc,distM=daisy.mat,minClusterSize=2)
df$clusters = cutreeHybrid(hc,distM=daisy.mat,minClusterSize=2)$labels
df
df[,c(cols,"clusters")]

df.rand = data.rand %>% unite(lang, cols,sep="",remove=FALSE)
daisy.mat.rand <- 1-stringsimmatrix(df.rand$lang,df.rand$lang,useNames="strings",method="lv")
hc.rand <- hclust(as.dist(daisy.mat.rand),method="complete")
plot(hc.rand)
plot(hc)

head(daisy.mat)

head(data)

require(vegan)
head(df)
plot(hc)

fviz_dist(as.dist(daisy.mat), lab_size = 8)

hcRange <- as.clustrange(hc, diss=as.dist(daisy.mat), ncluster=20) 
median((summary(hcRange)[1])[,1])
hcRange
plot(hc)

plot(hcRange, stat = c("ASWw", "HG", "PBC"), lwd = 2)


require(vegan)

create.plots.all = function(cols=c(),controller=TRUE){
   require(jsonlite)

   print(paste("Starting Resouce Type: ",paste(cols,collapse="")))
   counter = 0
   pb = txtProgressBar(min = 0, max = 11*10*11*4, initial = 0,style=3) 
   plot_list = list()

   for (agent_no in c(10:1,0.5)){

      agents = invisible(stream_in(file(paste('./wordlists/',paste(cols,collapse=""),'/wordList_',agent_no*50,'.json',sep="")),verbose=FALSE))
      agents$hashcode = !is.na(agents$hashcode)
      
      res_list = list()

      for (res in c(1:4)*500){
         env_list <- list()
      for (env in unique(agents$environment)){
         trial_list <- list()
         for (trl in unique(agents$trial)){

            data=subset(agents,hashcode==controller &resources==res&environment==env & trial==trl)
            counter = counter + 1

            setTxtProgressBar(pb,counter)


            flag <- TRUE
            tryCatch({trial_list[[trl+1]] <- create.cluster.plot.custom(data,TRUE,cols,gower=FALSE,create.plot=FALSE)}, error=function(e) flag<<-FALSE)
            if (!flag){

               write(paste("fail on trial=",trl," environment=",env," resources=",res," agents=",agent_no," type=",paste(cols,collapse=""),collapse=""),file="plot_log.txt",append=TRUE)

               next
            } 

         }
      env_list[[env+1]] <- trial_list
      }
      res_list[[res]] <- env_list
      }
      plot_list[[agent_no*50]] <- res_list
      gc(verbose=FALSE)

   }

   print("Done")

   return(plot_list)

}


create.similarity.df = function(cols,ANN=TRUE){

   require(jsonlite)
   require(dplyr)
   require(tidyverse)
   require(stringdist)
   sim_df = data.frame()

   counter = 0
   pb = txtProgressBar(min = 0, max = 11*10*11*4, initial = 0,style=3) 
      
   for (agent_no in c(10:1,0.5)*50){

      agents = stream_in(file(paste('./wordlists/',paste(cols,collapse=""),'/wordList_',agent_no,'.json',sep="")),verbose=FALSE)
      agents$hashcode = !is.na(agents$hashcode)

      for (res in c(1:4)*500){

      vec <- c()
      for (env in 0:9){

         for (trl in 0:10){
            
            data=subset(agents,hashcode==ANN &resources==res&environment==env & trial==trl)


            df = data %>% unite(lang, cols,sep="",remove=FALSE)
            daisy.mat <- stringsimmatrix(df$lang,df$lang,useNames="strings",method="lv")

            vec <- c(vec,as.vector(rowMeans(daisy.mat)))

            counter = counter + 1

            setTxtProgressBar(pb,counter)


         }
      }
      gc(verbose=FALSE)


      t = dplyr::bind_cols(confidence_interval(vec,0.95),q1 = quantile(vec,0.25), median = median(vec),q3=quantile(vec,0.75),agents=agent_no,res=res)
      sim_df = dplyr::bind_rows(sim_df,t)
   }

   sim_df$type = paste(cols,collapse="")
      
   }

   return(sim_df)


}


cluster.data.frame = function(cols,controller=TRUE){

   require(jsonlite)
   require(dplyr)
   require(tidyverse)
   require(stringdist)
   require(dynamicTreeCut)

   sim_df = data.frame()

   # pb = txtProgressBar(min = 0, max = 21*10*11*4, initial = 0,style=3) 
      
   for (agent_no in c(10:1,0.5,20)*50){

      agents = stream_in(file(paste('./wordlists/',paste(cols,collapse=""),'/wordList_',agent_no,'.json',sep="")),verbose=FALSE)
      agents$hashcode = !is.na(agents$hashcode)
      agents$population=agent_no

      for (res in c(1:5)*500){

      vec <- c()
      for (env in 0:9){


         r = foreach (trl=0:20,.combine=rbind) %dopar%{
            
            data=subset(agents,hashcode==controller &resources==res&environment==env & trial==trl)

            df = data %>% unite(lang, cols,sep="",remove=FALSE)
            daisy.mat <- 1-stringsimmatrix(df$lang,df$lang,useNames="strings",method="lv")
            hc <- hclust(as.dist(daisy.mat),method="complete")
            data$cluster = cutreeHybrid(hc,distM=daisy.mat,minClusterSize=2,verbose=0)$labels
            as.data.frame(data)
         }
         sim_df = dplyr::bind_rows(sim_df,r)

      }
      gc(verbose=FALSE)

   }

   sim_df$type = paste(cols,collapse="")
      
   }

   return(sim_df)

}


cluster.sim.df = function(data,cols){

   data = data[ ,c(cols,"fitness")]

   df = data %>% unite(lang, cols,sep="",remove=FALSE)
   daisy.mat <- 1-stringsimmatrix(df$lang,df$lang,useNames="strings",method="lv") #()
   daisy.mat <- scale(daisy.mat)

   vec <- c()
   methods = c("silhouette", "wss")
   for ( m in 1:length(methods)){
      n_clust<-fviz_nbclust(daisy.mat, kmeans, method = methods[m],k.max = 10)
      n_clust<-n_clust$data
      vec[m]<-as.numeric(n_clust$clusters[which.max(n_clust$y)])
   }

   gap <- clusGap(daisy.mat, kmeans, K.max=10, B=50)
   vec[3] <- maxSE(gap$Tab[, "gap"], gap$Tab[, "SE.sim"], method="Tibs2001SEmax")

   clusterno = max(vec)

   res <- kmeans(daisy.mat,clusterno)

   data$cluster=factor(res$cluster)

   centers = as.data.frame(res$centers)

   centers$cluster = factor(seq.int(nrow(res$centers)))

   df.stats = data.frame()

   for (i in 1:nrow(res$centers)){
   df <- read.csv(text=paste(cols,collapse=","),header=TRUE)
   row = c()
   for (term in c(1:length(cols))){

      term_list <- sort(summary(as.factor(data[res$cluster==i,term])),decreasing=TRUE)

      if (names(term_list[1]) == "(Other)"){
      vec <- term_list[2]

      } else{
      vec <- term_list[1]

      }

      row[term]=vec[[1]]/table(res$cluster)[[i]]

   }


   proportion.pop = table(res$cluster)[[i]]/length(data[,1])
   fitness.mean = mean(data[res$cluster==i,"fitness"])
   fitness.sd = sd(data[res$cluster==i,"fitness"])
   df[nrow(df)+1,] = row
   df = cbind(df,prop=proportion.pop,mean=fitness.mean,sd=fitness.sd)
   df.stats <- rbind(df,df.stats)

   }

   result = list()
   result[["kmeans"]] = res
   result[["df"]] = df.stats 

   return(df.stats)

}