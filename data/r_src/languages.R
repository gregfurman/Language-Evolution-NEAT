library(tidyverse)
library(stringdist)
install.packages("tidyverse")

language.stats = function(data,cols){

   words = plyr::count(data,cols)
   row = head(words[order(-words$freq),],unique(data$population))

   row = row %>% unite(lang, cols,sep="-",remove=FALSE)
   data = data %>% unite(lang, cols,sep="-",remove=FALSE)

   data.subset = subset(data,lang %in% row$lang)
   data.mean = aggregate(data=data.subset,fitness ~ lang,mean)
   df = left_join(data.mean, row, by=c("lang"))

   df$env = rep(unique(data$environment),length(df[,1]))
   df$res = rep(unique(data$resources),length(df[,1]))
   df$population = rep(unique(data$population),length(df[,1]))
   df$trial = rep(unique(data$trial),length(df[,1]))

   return(df)
   
}

language.stats.agents = function(data,cols){

   df = data.frame()

   for (env in unique(data$environment)){

      for (res in unique(data$resources)){

         for (trl in unique(data$trial)){
            
            data.subset = subset(data,environment==env&resources==res&trial==trl)
            df = rbind(df,language.stats(data.subset,cols))
         
         }
      }
   }
   
   return(df)

}

language.stats.all = function(cols){


   df = data.frame()

   for (agent in c(0.5,1:10)*50){

      agents = stream_in(file(paste('wordlists/',paste(cols,collapse=""),'/wordList_',agent,'.json',sep="")))

      # agents = stream_in(file(paste('wordlists/length_test/wordList_',agent,'.json',sep="")))
      agents$hashcode = !is.na(agents$hashcode)
      agents = subset(agents,hashcode)
      agents[cols] <- lapply(agents[cols], factor)  ## as.factor() could also be used

      agents$population = rep(agent,length(agents[,1]))
      df=rbind(df,language.stats.agents(agents,cols))

      print(tail(df))
      # write.csv(df,"wordsTest.txt",row.names=FALSE)

   }

   return(df)

}

agent.total.stats = function(cols){

   df = data.frame()

   for (agent in c(0.5,1:10)*50){

      agents = stream_in(file(paste('wordlists/',paste(cols,collapse=""),'/wordList_',agent,'.json',sep="")))
      agents$hashcode = !is.na(agents$hashcode)
      agents[cols] <- lapply(agents[cols], factor)  ## as.factor() could also be used
      agents$population = rep(agent,length(agents[,1]))
      agents$type = rep(paste(cols,collapse=""),length(agents[,1])) 


      df=rbind(df,agents)

   }

   return(df)



}

simulation.fitness = function(cols){

   df = data.frame()

   for (agent in c(0.5,1:10)*50){

      agents = stream_in(file(paste('wordlists/',paste(cols,collapse=""),'/wordList_',agent,'.json',sep="")))
      agents$hashcode = !is.na(agents$hashcode)
      agents[cols] <- lapply(agents[cols], factor)  ## as.factor() could also be used
      agents$population = rep(agent,length(agents[,1]))
      agents$type = rep(paste(cols,collapse=""),length(agents[,1])) 


      df=rbind(df,agents)

   }

   

   return(df)



}




lang.summary = function(cols){

df = data.frame()

for (agent in c(1:3)*50){

   agents = stream_in(file(paste('wordlists/',paste(cols,collapse=""),'/wordList_',agent,'.json',sep="")))
   agents$hashcode = !is.na(agents$hashcode)
   agents[cols] <- lapply(agents[cols], factor)  ## as.factor() could also be used
   agents$population = rep(agent,length(agents[,1]))
   agents$type = rep(paste(cols,collapse=""),length(agents[,1])) 

   agents = agents %>% unite(lang, cols,sep="-",remove=FALSE)

   df <- as.data.frame(summary(factor(agents$lang),maxsum = max(lengths(lapply(agents[cols], unique)))))
   colnames(df) = c("count")
   df <- tibble::rownames_to_column(df, "lang")
   df <- subset(df,lang != "(Other)")


   df=rbind(df,agents)

   }

}