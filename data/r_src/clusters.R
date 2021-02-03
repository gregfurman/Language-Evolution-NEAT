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

for (vec in list(c(0:4),c(5:9),c(10:15),c(16:20))){
   print(vec)
}


0 1 2 3 4
5 6 7 8 9
10 11 12 13 14
15 16 17 18 19

p = data.frame(A="a",B="b")

p %>% tidyr::unite(lang,c(A,B))

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

require(devtools)
withr::with_libpaths(new = "./package", install("package/tidyverse", dependencies = TRUE))

library(miniCRAN)

install.packages(pkgs, repos = paste0("./packages/src/contrib"),type = "source")

tidyr::unite()

pkgs <- pkgDep("tidyverse")
pkgList <- pkgDep(pkgs, type = "source", suggests = FALSE)
makeRepo(pkgList, path="package/.", type = c("source"))

install.packages("miniCRAN")

cluster.data.frame = function(cols,controller=TRUE){

   require(devtools)

   load_all('./package/stringdist/pkg')
   load_all('./package/dbplyr')
   load_all('./package/modelr')
   load_all('./package/reprex')
   load_all('./package/rvest')
   load_all('./package/tidyverse')

   require(parallel)
   require(doParallel)
   require(foreach)
   require(jsonlite)
   require(dplyr)
   # require(tidyverse)
   # require(stringdist)
   require(dynamicTreeCut)

   sim_df = data.frame()
      
   for (agent_no in c(1)*50){

      # agents = stream_in(file(paste('./wordlists/',paste(cols,collapse=""),'/wordList_',agent_no,'.json',sep="")))
      agents = stream_in(file(paste('wordList_',agent_no,'.json',sep="")))

      agents$hashcode = !is.na(agents$hashcode)
      agents$population=agent_no

      for (res in c(1:5)*500){

         for (dim in c(50,75,100,150)^2){

            for (splt in c(0.1,0.2,0.3,0.4,0.5)){

            vec <- c()
            for (env in 0:9){

               registerDoParallel(4)
               r = foreach (trl=0:20,.combine=rbind) %dopar%{
                  
                  data=subset(agents,hashcode==controller &resources==res&environment==env & trial==trl&split==splt&world==dim)

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
         }
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


p = data.frame(x=1:10,y=11)

as.dist(p$y)

args <- commandArgs(trailingOnly = TRUE)
agent_no = as.numeric(args[1])

cluster.data.frame = function(cols,controller=TRUE,agent_no=25){
   require(devtools)
   require(stringdist)
   require(tidyverse)
   # setwd("./package/src/contrib")
   # pkgs <- list.dirs('.', recursive=FALSE)

   # sapply(pkgs, require, character.only = TRUE)

   # setwd("../../..")

   # load_all('./package/stringdist/pkg')
   
   require(parallel)
   require(doParallel)
   require(foreach)
   require(jsonlite)
   require(dplyr)
   
   require(dynamicTreeCut)

   sim_df = data.frame()
      
   

      agents = stream_in(file(paste('wordlists/final/wordList_',agent_no,'.json',sep="")),verbose=TRUE)
      agents$hashcode = !is.na(agents$hashcode)
      agents$population=agent_no

      for (res in c(1:5)*500){

         for (dim in c(50,75,100,150)^2){

            for (splt in c(0.1,0.2,0.3,0.4,0.5)){

            vec <- c()
            for (env in 0:9){

               #registerDoParallel(10)
		#for (comb in list(c(0:4),c(5:9),c(10:15),c(16:20))){
               	#r = foreach (trl=comb,.combine=rbind) %dopar%{
                 for (trl in c(0:20)){  
                  data=subset(agents,hashcode==controller &resources==res&environment==env & trial==trl&split==splt&world==dim)
                  print(head(data))
                  if (nrow(data))
                  data$cluster <- NA
                  df = data %>% tidyr::unite(lang, cols,sep="",remove=FALSE)
                  if (length(unique(df$lang))>1){
                     daisy.mat <- 1-stringsimmatrix(df$lang,df$lang,useNames="strings",method="lv")
                     hc <- hclust(as.dist(daisy.mat),method="complete")
                     data$cluster = cutreeHybrid(hc,distM=daisy.mat,minClusterSize=2,verbose=0)$labels
                     rm(hc,daisy.mat,df)
                  } else{
                   data$cluster=0                     
                  }

  		  sim_df = dplyr::bind_rows(sim_df,data)
                  rm(data)
                  gc(verbose=FALSE)
                  #as.data.frame(data)
               }
                #sim_df = dplyr::bind_rows(sim_df,r)
             }
            	gc(verbose=FALSE)
	  }
            
           }
}       
   sim_df$type = paste(cols,collapse="")
   return(sim_df)

}



head(df)

ggplot(df, aes(x=resources,y=fitness)) + geom_point()

head(subset(df,resources==1500))

unique(df$split)

agent_no = as.numeric(args[1])
df = cluster.data.frame(c("A","B"))
save(df,file=paste(paste("AB_controller",agent_no,sep="_"),".RData",sep=""))
