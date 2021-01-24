## HCLUST RESULTS ################

load("agent_data.RData")

require(ggplot2)
require(dplyr)


data.summary = agent_data %>%
  group_by(trial,type,environment,controller,resources,population) %>%
  summarise(n = max(cluster)) %>%
    group_by(type,controller,resources,population) %>%
   summarise(mean = mean(n), sd= sd(n),q1 = quantile(n,0.25),q3 = quantile(n,0.75),median=median(n), n=n())

## Normality Tests #############

shapiro.test(filter(df,population==350)$ANN)
ks.test((filter(cleaned.data, population==350 & controller=="ANN")$n-filter(cleaned.data, population==350 & controller=="Random")$n)
, "pnorm", mean=mean((filter(cleaned.data, population==350 & controller=="ANN")$n-filter(cleaned.data, population==350 & controller=="Random")$n)
), sd=sd((filter(cleaned.data, population==350 & controller=="ANN")$n-filter(cleaned.data, population==350 & controller=="Random")$n)
))
hist(filter(df,population==350)$ANN)

diff = filter(cleaned.data, population==350 & controller=="ANN")$n-filter(cleaned.data, population==350 & controller=="Random")$n

t.test(filter(cleaned.data, population==350 & controller=="ANN")$n-filter(cleaned.data, population==350 & controller=="Random")$n)


qqnorm(filter(df,population==500)$diff, pch = 1, frame = FALSE)
qqline(filter(df,population==500)$diff, col = "steelblue", lwd = 2)


cor(cleaned.data$n,cleaned.data$fitness)

plot(scale(cleaned.data$n),scale(cleaned.data$fitness))

library(ggfortify)
res.pca <- prcomp(subset(cleaned.data,controller=="ANN")[,c("resources", "population","n","types","fitness")], scale = TRUE)
res.pca <- prcomp(subset(cleaned.data,controller=="ANN")[,c( "population","n","fitness")], scale = TRUE)


autoplot(res.pca, data = iris, colour = 'population',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)


fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )


head(subset(cleaned.data,controller=="ANN")[,c("resources", "population","n", "fitness","types")])
library(car)

rglwidget()
scatter3d(fitness ~ population + resources | factor(controller), cleaned.data)

ggplot(cleaned.data, aes(x=population,y=fitness,colour=factor(controller)))+ 
stat_summary(fun=mean,geom="line",size=0.8) +
stat_summary(fun=mean,geom="point",size=0.8) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) 

summary(aov(fitness ~ factor(population) + factor(controller) + factor(type) + n,cleaned.data))

ggplot(subset(cleaned.data,population==1000), aes(x=(n),y=fitness,colour=factor(population),fill=factor(population)))+ #geom_point(alpha=0.05)
stat_summary(fun=mean,geom="line",size=0.8) +
stat_summary(fun=mean,geom="point",size=0.8) +
  stat_summary(geom = "ribbon", fun.data = mean_se,alpha=0.2,size=0) 


ggplot(cleaned.data, aes(x=resources,y=fitness,colour=factor(controller)))+ 
stat_summary(fun=mean,geom="line",size=0.8) +
stat_summary(fun=mean,geom="point",size=0.8) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) 


ggplot(cleaned.data, aes(x=population,y=fitness))+ 
stat_summary(fun=mean,geom="line",size=0.8) 
   # stat_summary(fun=mean, geom="point",alpha=0.5,size=2) 

## Modelling #############
library(MASS)
library(stargazer)
library(dplyr)
cleaned.data=agent_data %>%
  group_by(trial,type,environment,controller,resources,population) %>%
  summarise(n = max(cluster), fitness=mean(fitness))

save(cleaned.data,file="cleaned_data.RData")
load("cleaned_data.RData")

library(effects)
install.packages("effects")

plot(effect(term="controller:population",mod=linear.model,default.levels=20),multiline=TRUE)




tail(cleaned.data)
cleaned.data$types=nchar(cleaned.data$type)

head(cleaned.data["controller","resources", "population","n","types",])

linear.model <- lm(data=cleaned.data, n ~controller*(population + resources + types) )
linear.model.2 <- lm(data=cleaned.data, fitness ~controller+population+resources+types )
linear.model.3 <- lm(data=cleaned.data, n ~controller*fitness*(population + resources + types) )


plot(filter(cleaned.data,controller=="ANN")$fitness,filter(cleaned.data,controller=="Random")$fitness)
plot(filter(cleaned.data,controller=="ANN")$n,filter(cleaned.data,controller=="Random")$n)

summary(lm(data=cleaned.data, fitness ~ controller))

summary(linear.model.2)

predict(linear.model, data.frame(controller="ANN",resources=500,types=3,population=25))
predict(linear.model, data.frame(controller="Random",resources=500,types=3,population=25))


head(k)
step.model <- stepAIC(linear.model, direction = "both", 
                      trace = FALSE)
summary(linear.model.3)

k$types = nchar(k$type)
require(stargazer)
stargazer(linear.model,keep.stat=c("n","adj.rsq","f"),df=FALSE,type="text", dep.var.labels="Amount of Language Clusters",
covariate.labels=c("No Controller","Population Size","Resource Amount","No. of Resource Types","No Controller X Population Size","No Controller X Resource Amount","No Controller X Resource Types"),ci=TRUE, ci.level=0.90, single.row=TRUE)

correlation.matrix <- cor(cleaned.data[,c("n","resources", "population","types")])
stargazer(correlation.matrix, title="Correlation Matrix",dep.var.labels="Amount of Language Clusters",covariate.labels=c("Population Size","Resource Amount","No. of Resource Types"),type="text")

predict(step.model, data.frame(controller="ANN",population=200,resources=500,type=2))
summ(step.model)
anova(step.model)
write.csv(broom::tidy(step.model),"lm.csv",row.names=FALSE)

poisson.model <- glm(n/population ~ controller + population + resources + type, cleaned.data,family = quasipoisson)
summary(poisson.model)



plot(poisson.model)

## Model interpretation and comparison


require(AER)
require(performance)
require(MASS)


sqrt(mean((predict(poisson.model,cleaned.data,type="response")-cleaned.data$n/cleaned.data$population)^2))
sqrt(mean((predict(linear.model,cleaned.data,type="response")-cleaned.data$n)^2))
sqrt(mean((predict(step.model,cleaned.data,type="response")-cleaned.data$n)^2))
summary(step.model)


cleaned.data
##### Plots ######################

title_size = 15
lab_size = 10

cluster_description = "\nClusters were formed using Complete Linkage Clustering of a Levenshtein dissimilarity matrix\n\nError Bars represent Standard Error."


## Controller Plots #############

g<-ggplot(cleaned.data, aes(y=n,x=factor(type),colour=controller)) + 
stat_summary(fun=mean, geom="line",aes(group=controller),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller),alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0) +
       labs(x="Resource Types",y="Cluster Amount",colour="Controller",fill="Controller",title=" Average Amount of Clusters Formed per Resource Type") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g


ggsave("clusters_vs_types_by_controller.png",g)
library(zoo)
library(ggplot2)
library(dplyr)

g<-cleaned.data%>%
ggplot(aes(x=n,y=(fitness),colour=factor(controller),fill=factor(controller)))+ 
stat_summary(fun=mean,geom="line",size=0.8,alpha=0.8) +
stat_summary(fun=mean,geom="point",size=0.8,alpha=0.8) +
  stat_summary(geom = "ribbon", fun.data = mean_se,alpha=0.1,size=0) +
  geom_smooth(method="lm")+
   labs(x="Cluster Amount",y="Fitness",colour="Controller",fill="Controller",title=" Average Fitness per Cluster Amount by Controller") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("fitness_v_clusters_by_contoller.png",g)

cor.test(cleaned.data$n,cleaned.data$fitness,method="kendall")
t.test(fitness ~ controller, cleaned.data)

cor(filter(cleaned.data,controller=="ANN")$fitness,filter(cleaned.data,controller=="Random")$fitness)
g<-cleaned.data%>%
ggplot(aes(x=resources,y=(fitness),colour=factor(controller),fill=factor(controller)))+ 
stat_summary(fun=mean,geom="line",size=0.8,alpha=0.8) +
stat_summary(fun=mean,geom="point",size=0.8,alpha=0.8) +
  stat_summary(geom = "ribbon", fun.data = mean_se,alpha=0.1,size=0) +
   labs(x="Resources",y="Fitness",colour="Controller",fill="Controller",title=" Average Fitness per Resource amount by Controller") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

g<-cleaned.data%>%
ggplot(aes(x=population,y=(fitness),colour=factor(controller),fill=factor(controller)))+ 
stat_summary(fun=mean,geom="line",size=0.8,alpha=0.3) +
stat_summary(fun=mean,geom="point",size=0.8,alpha=0.3) +
  stat_summary(geom = "ribbon", fun.data = mean_se,alpha=0.1,size=0) +
   labs(x="Population",y="Fitness",colour="Controller",fill="Controller",title=" Average Fitness per Cluster Amount by Controller") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g



g<-cleaned.data%>%
ggplot(aes(x=resources,y=(fitness),colour=factor(controller),fill=factor(controller)))+ 
stat_summary(fun=mean,geom="line",size=0.8,alpha=0.3) +
stat_summary(fun=mean,geom="point",size=0.8,alpha=0.3) +
  stat_summary(geom = "ribbon", fun.data = mean_se,alpha=0.1,size=0) +
   labs(x="Resources",y="Fitness",colour="Controller",fill="Controller",title=" Average Fitness per Cluster Amount by Controller") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g
ma <- function(x, n = 5){stats::filter(x, rep(1 / n, n), sides = 2)}


g<-ggplot(cleaned.data, aes(x=population,y=(fitness),colour=factor(controller),fill=factor(controller)))+ 
stat_summary(fun=mean,geom="line",size=0.8) +
stat_summary(fun=mean,geom="point",size=0.8) +
  stat_summary(geom = "ribbon", fun.data = mean_se,alpha=0.2,size=0) +
   labs(x="Population",y="Fitness",colour="Controller",fill="Controller",title=" Average Fitness per Cluster Amount by Controller") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g



g <- ggplot(data=cleaned.data, aes(x=(population), y=n,colour=controller),alpha=0.4) + 
 stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0,alpha=0.8) +
 labs(x="Population",y="Clusters",colour="Controller",fill="Controller",title="Average Amount of Clusters per\nPopulation Size",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusters_vs_pop_by_controller.png",g)

g <- ggplot(data=cleaned.data, aes(x=(population), y=n,colour=controller),alpha=0.4) + 
 stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0,alpha=0.8) +
 labs(x="Population",y="Clusters",colour="Controller",fill="Controller",title="Average Amount of Clusters per\nPopulation Size",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g



write.csv(pairwise.t.test(cleaned.data$n,cleaned.data$type,paired=TRUE,p.adjust.method="bonferroni")$p.value,"test.csv")
pairwise.t.test(cleaned.data$n,cleaned.data$type,paired=TRUE,p.adjust.method="bonferroni")
t.test(filter(cleaned.data,type=="ABCD")$n,filter(cleaned.data,type=="ABCDE")$n)

g <- ggplot(data=cleaned.data, aes(x=(population), y=n/population,colour=controller),alpha=0.4) + 
 stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0,alpha=0.8) +
 labs(x="Population",y="Cluster Per Agent",colour="Controller",fill="Controller",title="Average Amount of Clusters per Agent\nper Population Size",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))

g

ggsave("cluster_per_agent_vs_population_by_controller.png",g)


g <- ggplot(data=cleaned.data, aes(x=(population), y=population/n,colour=controller),alpha=0.4) + 
 stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0,alpha=0.8) +
 labs(x="Population",y="Agents Per Cluster",colour="Controller",fill="Controller",title="Average Amount of Agents per Cluster\nper Population Size",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))

g

ggsave("agent_per_cluster_vs_population_by_controller.png",g)

g<- cleaned.data %>% group_by(resources,controller) %>%# summarize(mean=mean(n), sd=sd(n), n=n(),se=sd/sqrt(n)) %>%
ggplot(aes(x=resources, y=n,colour=controller),alpha=0.4) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
 stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0,alpha=0.8) +
 labs(x="Resource Amount",y="Clusters",colour="Controller",fill="Controller",title="Average Amount of Clusters\nper Resource Amount",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusters_vs_resources_by_controller.png",g)

## Resource Plots #############



g<-ggplot(cleaned.data, aes(y=n,x=population,group=factor(resources),colour=factor(resources))) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Population",y="Clusters",colour="Resource Amount",title=" Average Amount of Clusters per Agent vs Population",caption=cluster_description) +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clusters_vs_population_by_resources.png",g)

g<-ggplot(cleaned.data, aes(y=n/population,x=population,group=factor(resources),colour=factor(resources))) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Population",y="Cluster Per Agent",colour="Resource Amount",title=" Average Amount of Clusters per Agent vs Population",caption=paste(cluster_description,"\n\nWhere Cluster per Agent = (Cluster Amount) / (Population)")) +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("cluster_per_agent_vs_population_by_resources.png",g)


## Language Type Plots ######

g<-ggplot(cleaned.data, aes(y=n,x=population,group=factor(type),colour=factor(type))) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Population",y="Cluster",colour="Resource Types",title=" Average Amount of Clusters vs Population") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("cluster_vs_population_by_type.png",g)


g<-ggplot(cleaned.data, aes(y=n/population,x=population,group=factor(type),colour=factor(type))) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Population",y="Cluster per Agent",colour="Resource Types",title=" Average Amount of Clusters per Agent vs Population") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("cluster_per_agent_vs_population_by_type.png",g)



g<-ggplot(cleaned.data, aes(y=n,x=population)) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Population",y="Cluster",colour="Resource Types",title=" Average Amount of Clusters vs Population") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clusters_vs_population.png",g)


g<-ggplot(cleaned.data, aes(y=n/population,x=population)) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Population",y="Cluster per Agent",colour="Resource Types",title=" Average Amount of Clusters per Agent vs Population") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("cluster_per_agent_vs_population.png",g)

g<-ggplot(cleaned.data, aes(y=n,x=resources)) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Resource Amount",y="Cluster",colour="Resource Types",title=" Average Amount of Clusters vs Resource Amount") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clusters_vs_resources.png",g)


g<-ggplot(cleaned.data, aes(y=n,x=resources,colour=type,group=type)) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Resource Amount",y="Cluster",colour="Resource Types",title=" Average Amount of Clusters vs Resource Amount") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clusters_vs_resources_by_type.png",g)

g<-ggplot(cleaned.data, aes(y=n/population,x=resources,colour=type,group=type)) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Resource Amount",y="Cluster per Agent",colour="Resource Types",title=" Average Amount of Clusters per Agent vs Resource Amount") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("cluster_per_agent_vs_resources_by_type.png",g)

g<-ggplot(cleaned.data, aes(y=n/population,x=resources)) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Resource Amount",y="Cluster",colour="Resource Types",title=" Average Amount of Clusters vs Resource Amount") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clusters_per_agent_vs_resources.png",g)

g<-ggplot(cleaned.data, aes(y=n,x=factor(type))) + 
stat_summary(fun=mean, geom="line",aes(group=1),size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +
       labs(x="Resource Amount",y="Cluster",colour="Resource Types",title=" Average Amount of Clusters vs Resource Amount") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clusters_vs_resource_types.png",g)

g<-ggplot(cleaned.data, aes(y=n,x=population,colour=factor(type))) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=2) +
       labs(x="Population",y="Cluster",colour="Resource Types",title=" Average Amount of Clusters vs Population size") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clusters_vs_population_by_resource_types.png",g)

## Boxplots #################

library(ggplot2)
library(ggpubr)
library(rstatix)

g<-ggplot(cleaned.data, aes(y=n,x=factor(controller),colour=factor(controller))) + 
geom_boxplot()+
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
       labs(x="Controller",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Controller Type") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g


g<-ggplot(cleaned.data, aes(y=n,x=controller)) + 
# geom_boxplot()+
   stat_summary(fun=mean, geom="line",group=1,alpha=1,size=0.8) +

   stat_summary(fun=mean, geom="point",group=1,alpha=1,size=2) +
     stat_summary(geom = "errorbar", fun.data = mean_se,width=0) +

       labs(x="Controller",y="Clusters",colour="Controller",title=" Plot of Average Cluster Amounts vs Controller Type") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g      

ggsave("cluster_amount_v_controller.png",g)
t.test(n ~ controller, cleaned.data,paired=TRUE)

bxp <-ggpaired(cleaned.data, x = "controller", y = "n", 
         order = c("Random", "ANN"),
         ylab = "Amount of Clusters Formed", xlab = "Controller")


stat.test <- cleaned.data %>% group_by() %>%
  t_test(n ~ controller, paired = TRUE) %>%
  add_significance()

stat.test = stat.test %>% add_xy_position(x = "Controller")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed= TRUE))
bxp

ggsave("boxplot_clusters_v_controller.png",g)


g<-ggplot(cleaned.data, aes(y=n,x=factor(controller),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Controller",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Controller Type") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("boxplot_clusters_v_population_by_controller.png",g)


g<-ggplot(cleaned.data, aes(y=n/population,x=factor(controller),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Controller",y="Clusters",colour="Controller",title=" Boxplots of Cluster Per Agent vs Controller Type") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("boxplot_clusters_per_agent_by_controller.png",g)


g<-ggplot(cleaned.data, aes(y=n/population,x=factor(population),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Controller",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Population") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("boxplot_clusters_v_population_by_controller.png",g)


cleaned.data %>% group_by(controller) %>%
   summarise(mean = mean(n), sd= sd(n),q1 = quantile(n,0.25),q3 = quantile(n,0.75),median=median(n), n=n())


cleaned.data %>% group_by(population) %>%
   summarise(mean = mean(n), sd= sd(n),q1 = quantile(n,0.25),q3 = quantile(n,0.75),median=median(n), n=n())


cleaned.data %>% group_by(resources) %>%
   summarise(mean = mean(n), sd= sd(n),q1 = quantile(n,0.25),q3 = quantile(n,0.75),median=median(n), n=n())

g<-ggplot(cleaned.data, aes(y=n,x=factor(type),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Resources Types",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Resource Types") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g


g<-ggplot(cleaned.data, aes(y=n,x=factor(type))) + 
geom_boxplot()+
       labs(x="Resources Types",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Resource Types") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

cleaned.data %>% group_by(type) %>%
   summarise(mean = mean(n), sd= sd(n),q1 = quantile(n,0.25),q3 = quantile(n,0.75),median=median(n), n=n())




###########################

w = 2 * IQR((cleaned.data$n)) / length(cleaned.data$n)^(1/3)
w
g <- ggplot(cleaned.data,aes(y=..density..,,x=n,fill=controller,color=controller)) +
  geom_histogram(alpha=0.5,binwidth = w) + 
    geom_density(adjust = 3,alpha=0.05)+
   #  geom_vline(aes(xintercept = mean),data=nn,linetype = 5) +
     facet_grid(controller~.) +
     labs(x="Proportion",y="Density",colour="Controller",fill="Controller",title=" Histogram of Proportion of Population within Clusters",caption="\nWhere the area under a density curve integrates to 1.") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g



w = 2 * IQR((cleaned.data$n)) / length(cleaned.data$n)^(1/3)
w
g <- ggplot(cleaned.data,aes(y=..density..,,x=n,fill=controller,color=controller)) +
  geom_histogram(alpha=0.5,binwidth = w,position = 'identity') + 
    geom_density(adjust = 3,alpha=0.1)+
   #  geom_vline(aes(xintercept = mean),data=nn,linetype = 5) +
   #   facet_grid(controller~.) +
     labs(x="Proportion",y="Density",colour="Controller",fill="Controller",title=" Histogram of Proportion of Population within Clusters",caption="\nWhere the area under a density curve integrates to 1.") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

############################

boot.t.test(x=filter(cleaned.data, population == 100 & controller=="ANN")$n, y=filter(cleaned.data, population == 100 & controller=="Random")$n,R=500,paired=TRUE)
boot.t.test(x=filter(cleaned.data, population == 100 & controller=="ANN")$n, y=filter(cleaned.data, population == 100 & controller=="Random")$n,R=500,paired=TRUE)


# agg = aggregate(data=cleaned.data, n ~ resources + controller,mean)
# friedman.test(y=agg$n,block=factor(agg$controller),group=factor(agg$resources))



kruskal.test(data=cleaned.data, n ~ resources)
pairwise.wilcox.test(cleaned.data$n, cleaned.data$resources, p.adj = "bonf")
cor.test(cleaned.data$n, cleaned.data$resources, 
                    method = "spearman")

pairwise.wilcox.test(cleaned.data$n, cleaned.data$resources, p.adj = "bonf")[["p.value"]]


# agg = aggregate(data=cleaned.data, n ~ population + controller,mean)
# friedman.test(y=agg$n,block=factor(agg$controller),group=factor(agg$population))
kruskal.test(data=cleaned.data, n ~ population)
pairwise.wilcox.test(cleaned.data$n, factor(cleaned.data$population), p.adj = "bonf")
cor.test(cleaned.data$n, cleaned.data$population, 
                    method = "spearman")

# agg = aggregate(data=cleaned.data, n ~ type + controller,mean)
# friedman.test(y=agg$n,group=factor(agg$type),block=factor(agg$controller))
kruskal.test(data=cleaned.data, n ~ type)
pairwise.wilcox.test(cleaned.data$n, factor(cleaned.data$type), p.adj = "bonf")
cor.test(cleaned.data$n, nchar(cleaned.data$type), 
                    method = "spearman")

###############################


## Controller Comparison Tables ###########

wilcox.test(filter(cleaned.data, controller=="ANN")$n,filter(cleaned.data, controller=="Random")$n)

group_by(cleaned.data, controller,type) %>%
  summarise(
    count = n(),
    median = median(n, na.rm = TRUE),
    IQR = IQR(n, na.rm = TRUE)
  )


a = cleaned.data[cleaned.data$controller=="ANN",]
colnames(a)[7] <- "ANN"
a=subset(a,select = -c(controller))

r = cleaned.data[cleaned.data$controller=="Random",]
colnames(r)[7] <- "Random"
r=subset(r,select = -c(controller))

df = merge(a,r)


a = k[k$controller=="ANN",]
colnames(a)[7] <- "ANN"
a=subset(a,select = -c(controller))

r = k[k$controller=="Random",]
colnames(r)[7] <- "Random"
r=subset(r,select = -c(controller))

df.k =merge(a,r)
gc()

df.pvals.pop = data.frame()
set.seed(800)
for (pop in c(0.5,1:10,20)*50) {
  
      p = subset(k,population==pop)

      ANN = dplyr::filter(p,controller=="ANN")$n
      Random = dplyr::filter(p,controller=="Random")$n

      r = boot.t.test(x=ANN,y=Random,R=500)

      t=data.frame(population=pop,
      diff.mean=mean(ANN)-mean(Random),
      pval = r[["boot.p.value"]],
      conf.lwr=r[["boot.conf.int"]][["2.5%"]],
      conf.upp=r[["boot.conf.int"]][["97.5%"]])
   
      df.pvals.pop = rbind(df.pvals.pop,t)

}
df.pvals.pop

write.csv(as.data.frame(df %>% summarise(median.ANN=median(ANN),median.Random=median(Random),IQR.ANN = IQR(ANN),IQR.Random = IQR(Random), p.value=wilcox.test(df$ANN,df$Random,alternative="greater")[["p.value"]])),"ANN_vs_Random.csv", row.names = FALSE)

t.test()
library(stargazer)
library(dplyr)
summary(av <- aov(n ~ (factor(population)+factor(resources)+factor(type)) *factor(controller),cleaned.data))
TUKEY <- TukeyHSD(x=av,conf.level=0.95)

t.test(data=k,n ~ controller)
library(rstatix)

head(k)

k %>% group_by(controller,population,resources,type,environment,trial) %>% summarise(mean= mean(n)) %>% group_by(population) %>%
  pairwise_t_test(
    mean ~ controller, paired = TRUE,
    p.adjust.method = "bonferroni",conf.level=0.95,detailed=TRUE
    ) %>%select(population,diff.mean=estimate,p.value=p.adj,conf.low,conf.high)


k %>% group_by(controller,population,resources,type,environment,trial) %>% summarise(mean= mean(n)) %>% group_by(resources) %>%
  pairwise_t_test(
    mean ~ controller, paired = TRUE,
    p.adjust.method = "bonferroni",conf.level=0.95,detailed=TRUE
    ) %>%select(resources,diff.mean=estimate,p.value=p.adj,conf.low,conf.high)

write.csv(k %>% group_by(controller,population,resources,type,environment,trial) %>% summarise(mean= mean(n)) %>% group_by(population) %>%
  pairwise_t_test(
    mean ~ controller, paired = TRUE,
    p.adjust.method = "bonferroni",conf.level=0.95,detailed=TRUE
    ) %>%select(population,diff.mean=estimate,p.value=p.adj,conf.low,conf.high),
   "ANN_vs_Random_amount_in_cluster_by_population.csv", row.names = FALSE)

write.csv(as.data.frame(k %>% group_by(controller,population,resources,type,environment,trial) %>% summarise(mean= mean(n)) %>% group_by(type) %>%
  pairwise_t_test(
    mean ~ controller, paired = TRUE,
    p.adjust.method = "bonferroni",conf.level=0.95,detailed=TRUE
    ) %>%select(type,diff.mean=estimate,p.value=p.adj,conf.low,conf.high)),
   "ANN_vs_Random_amount_in_cluster_by_type.csv", row.names = FALSE)


write.csv(as.data.frame(k %>% group_by(controller,population,resources,type,environment,trial) %>% summarise(mean= mean(n)) %>% group_by(resources) %>%
  pairwise_t_test(
    mean ~ controller, paired = TRUE,
    p.adjust.method = "bonferroni",conf.level=0.95,detailed=TRUE
    ) %>%select(resources,diff.mean=estimate,p.value=p.adj,conf.low,conf.high)),
    "ANN_vs_Random_amount_in_cluster_by_resources.csv", row.names = FALSE)



write.csv(as.data.frame(k %>% 
group_by(resources,environment, population,type,controller,trial) %>%
summarise(largest=max(n) %>%
group_by(type) %>%
  pairwise_t_test(
    mean ~ controller, paired = TRUE,
    p.adjust.method = "bonferroni",conf.level=0.95,detailed=TRUE
    ) %>%select(resources,diff.mean=estimate,p.value=p.adj,conf.low,conf.high)),
    "ANN_vs_Random_largest_amount_in_cluster_by_type.csv", row.names = FALSE))


write.csv(as.data.frame(k %>% 
group_by(resources,environment, population,type,controller,trial) %>%
summarise(largest=max(n) %>%
group_by(resources) %>%
  pairwise_t_test(
    mean ~ controller, paired = TRUE,
    p.adjust.method = "bonferroni",conf.level=0.95,detailed=TRUE
    ) %>%select(resources,diff.mean=estimate,p.value=p.adj,conf.low,conf.high)),
    "ANN_vs_Random_largest_amount_in_cluster_by_resources.csv", row.names = FALSE))

write.csv(as.data.frame(k %>% 
group_by(resources,environment, population,type,controller,trial) %>%
summarise(largest=max(n) %>%
group_by(population) %>%
  pairwise_t_test(
    mean ~ controller, paired = TRUE,
    p.adjust.method = "bonferroni",conf.level=0.95,detailed=TRUE
    ) %>%select(resources,diff.mean=estimate,p.value=p.adj,conf.low,conf.high)),
    "ANN_vs_Random_largest_amount_in_cluster_by_population.csv", row.names = FALSE))


k %>% 
group_by(resources,environment, population,type,controller,trial) %>%
summarise(largest=max(n) %>%




df <- tibble::rownames_to_column(cleaned.data, "id")
df$population = factor(df$population)
df$resources = factor(df$resources)
df$type = factor(df$type)

df <- df[,c("id","population","resources","type","controller","n")]
df %>% select(id,population,resources,type,controller,n)
head(df)

res <- df %>% rstatix::anova_test(dv = n, wid = id, within= c(population,resources,type,controller))
res

k %>% 
group_by(resources,environment, population,type,controller,trial) %>%
summarise(largest=max(n)

df %>%
  pairwise_t_test(
    n ~ population, paired = TRUE,
    p.adjust.method = "bonferroni"
    )

tibble::rownames_to_column(cleaned.data, "id") %>% group_by(controller)



k %>% group_by(population) %>% #summarise(mean= mean(n))
  pairwise_t_test(
    n ~ controller, paired = FALSE,
    p.adjust.method = "bonferroni"
    ) 

df %>% group_by(population) %>% do(tidy(t.test(.n ~ .controller,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(population,estimate,statistic,p.value,conf.low,conf.high)
df %>% group_by(resources) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(resources,estimate,statistic,p.value,conf.low,conf.high)
df %>% group_by(type) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(type,estimate,statistic,p.value,conf.low,conf.high)
df %>% group_by() %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(estimate,statistic,p.value,conf.low,conf.high)

column.labels = c("Population", "Mean Difference", "T-Stat","p.value","conf.low","conf.high")

df %>% group_by(population) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(population,estimate,p.value,conf.low,conf.high)


write.csv(as.data.frame(df %>% group_by(population) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(population,estimate,p.value,conf.low,conf.high),row.names=FALSE),"ANN_vs_Random_cluster_number_by_population.csv", row.names = FALSE)

write.csv(as.data.frame(df %>% group_by(resources) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(resources,estimate,p.value,conf.low,conf.high)
),"ANN_vs_Random_cluster_number_by_resources.csv", row.names = FALSE)

write.csv(as.data.frame(df %>% group_by(type) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(type,estimate,p.value,conf.low,conf.high)
),"ANN_vs_Random_cluster_number_by_type.csv", row.names = FALSE)

write.csv(as.data.frame(df %>% group_by(population) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(population,estimate,p.value,conf.low,conf.high),row.names=FALSE),"ANN_vs_Random_cluster_number_by_population.csv", row.names = FALSE)
write.csv(as.data.frame(df %>% group_by(population) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(population,estimate,p.value,conf.low,conf.high),row.names=FALSE),"ANN_vs_Random_cluster_number_by_population.csv", row.names = FALSE)

stargazer(as.data.frame(df %>% group_by(population) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(population,estimate,p.value,conf.low,conf.high)
),type="text",summary=FALSE)
stargazer(as.data.frame(df %>% group_by(resources) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(resources,estimate,statistic,p.value,conf.low,conf.high)
),type="text",summary=FALSE)

stargazer(as.data.frame(df %>% group_by(type) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(type,estimate,statistic,p.value,conf.low,conf.high)
),type="text",summary=FALSE)
stargazer(as.data.frame(df %>% group_by() %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(estimate,statistic,p.value,conf.low,conf.high)
),type="text",summary=FALSE)

library(dplyr)
library(tidyverse)
library(broom)
k$X = k$controller
k %>% group_by(population) %>% do(tidy(t.test(data=.,n ~ factor(X),mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(population,estimate,statistic,p.value,conf.low,conf.high)
df.k %>% group_by(resources) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(resources,estimate,statistic,p.value,conf.low,conf.high)
df.k %>% group_by(type) %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(type,estimate,statistic,p.value,conf.low,conf.high)
df.k %>% group_by() %>% do(tidy(t.test(.$ANN,.$Random,mu=0,paired=TRUE,p.adjust.method="bonferroni"))) %>% select(estimate,statistic,p.value,conf.low,conf.high)
head(k)
t.test(data=k, n ~ controller,mu=0,paired=TRUE,p.adjust.method="bonferroni")

head(k)


cleaned.data %>% group_by(n,controller,population)

ggplot(cleaned.data, aes(x=resources,y=n,group=factor(controller)))+
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=2) 

head(cleaned.data)
rownames(cleaned.data)
library(rstatix)
as.data.frame(f)
pairwise.t.test(filter(df,))

stargazer(TUKEY[["factor(type):factor(controller)"]], type = "text", summary = FALSE)
stargazer(TUKEY[["factor(resources):factor(controller)"]], type = "text", summary = FALSE)
stargazer(res[res[, 4] > 0.05, ], type = "text", summary = FALSE)
head(TUKEY[["factor(population):factor(controller)"]])
res <- TUKEY[["factor(population):factor(controller)"]]
res[res[, 4] > 0.05, ];

summary(av <- aov(n ~ (factor(resources)) *factor(controller),k))
summary(av)
TUKEY <- TukeyHSD(x=av,"factor(resources):factor(controller)",conf.level=0.95)
plot(TUKEY)
stargazer(TUKEY[["factor(resources):factor(controller)"]], type = "text", summary = FALSE)


stargazer(summary(av), type = "text", summary = FALSE)

stargazer(TUKEY[["factor(resources):factor(controller)"]], type = "text", summary = FALSE)
stargazer(TUKEY[["factor(resources)"]], type = "text", summary = FALSE)
stargazer(TUKEY[["factor(controller)"]], type = "text", summary = FALSE)

summary(av <- aov(n ~ (factor(population)) *factor(controller),k))
TUKEY <- TukeyHSD(x=av,conf.level=0.95)

stargazer(TUKEY[["factor(population):factor(controller)"]], type = "text", summary = FALSE)
stargazer(TUKEY[["factor(resources)"]], type = "text", summary = FALSE)
stargazer(TUKEY[["factor(population)"]], type = "text", summary = FALSE)


TUKEY

write.csv(df.pvals.type,"ANN_vs_Random_cluster_number_by_type.csv", row.names = FALSE)

df.pvals.pop = data.frame()
for (pop in c(0.5,1:10,20)*50) {
  
      k = subset(df,population==pop)
      r = wilcox.test(k$ANN,k$Random,alternative="greater")

      t=k %>%
      summarise(Population=pop,median.ANN=median(ANN),median.Random=median(Random),IQR.ANN = IQR(ANN),IQR.Random = IQR(Random), p.value=r[["p.value"]])
      df.pvals.pop = rbind(df.pvals.pop,t)
   
}
write.csv(df.pvals.pop,"ANN_vs_Random_cluster_number_by_population.csv", row.names = FALSE)
df.pvals.pop

g<-ggplot(cleaned.data, aes(y=n,x=factor(population),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Population",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Population Size") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g
ggsave("boxplot_clust_v_type_by_population.png",g)

df.pvals.pop

g<-ggplot(cleaned.data, aes(y=n,x=factor(controller),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Population",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Population Size") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

g<-ggplot(cleaned.data, aes(y=n,x=factor(controller),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Population",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Population Size") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

df.pvals.res = data.frame()
for (res in c(1:4)*500) {
      k = subset(df,resources==res)
      r = wilcox.test(k$ANN,k$Random,alternative="greater")

      t=k %>%
      summarise(Resources=res,median.ANN=median(ANN),median.Random=median(Random),IQR.ANN = IQR(ANN),IQR.Random = IQR(Random), p.value=r[["p.value"]])
      df.pvals.res = rbind(df.pvals.res,t)
}

df.pvals.res
r = boot.t.test(df$ANN,df$Random,R=500)

t=data.frame(
diff.mean=mean(df$ANN-df$Random),
pval = r[["boot.p.value"]],
conf.lwr=r[["boot.conf.int"]][["2.5%"]],
conf.upp=r[["boot.conf.int"]][["97.5%"]])
t
write.csv(df.pvals.res,"ANN_vs_Random_cluster_number_by_resources.csv", row.names = FALSE)
df.pvals.res
g<-ggplot(cleaned.data, aes(y=n,x=factor(resources),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Resources",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Resource Amount") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("boxplot_clust_v_res_by_controller.png",g)


df.pvals.res


df.pvals.type = data.frame()
for (x in c("A","AB","ABC","ABCD")) {
      k = subset(df,type==x)
      r = wilcox.test(k$ANN,k$Random,alternative="greater")

      t=k %>%
      summarise(type=x,median.ANN=median(ANN),median.Random=median(Random),IQR.ANN = IQR(ANN),IQR.Random = IQR(Random), p.value=r[["p.value"]])
      df.pvals.type = rbind(df.pvals.type,t)
}
write.csv(df.pvals.type,"ANN_vs_Random_cluster_number_by_types.csv", row.names = FALSE)
df.pvals.type

g<-ggplot(cleaned.data, aes(y=n,x=factor(type),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Resources Types",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Resource Types") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

g<-ggplot(cleaned.data, aes(y=n,x=factor(type),colour=factor(controller))) + 
geom_boxplot()+
       labs(x="Resources Types",y="Clusters",colour="Controller",title=" Boxplots of Cluster Amounts vs Resource Types") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("boxplot_clust_v_type_by_controller.png",g)




df.pvals.type

cleaned.data %>%
    group_by(controller) %>%
   summarise(mean = mean(n), sd= sd(n),Q1 = quantile(n,0.25),median=median(n),Q3 = quantile(n,0.75), n=n())

cleaned.data %>%
    group_by(type) %>%
   summarise(mean = mean(n), sd= sd(n),Q1 = quantile(n,0.25),median=median(n),Q3 = quantile(n,0.75), n=n())


temp =cleaned.data %>% unite("group",c("population","controller"),sep="-")
kruskal.test(data=temp, n ~ group)
pairwise.wilcox.test(temp$n,factor(temp$group),p.adj = "bonf")


temp =cleaned.data %>% unite("group",c("resources","controller"),sep="-")
kruskal.test(data=temp, n ~ group)
pairwise.wilcox.test(temp$n,factor(temp$group),p.adj = "bonf")

temp =cleaned.data %>% unite("group",c("type","controller"),sep="-")
kruskal.test(data=temp, n ~ group)
pairwise.wilcox.test(temp$n,factor(temp$group),p.adj = "bonf")

###############################

write.csv(pairwise.wilcox.test(cleaned.data.ann$n,factor(cleaned.data.ann$type),p.adj = "bonf")[["p.value"]],"test.csv", row.names = FALSE)



cleaned.data.ann = subset(cleaned.data, controller=="ANN")

boxplot(n ~ type, cleaned.data)

# temp =cleaned.data.ann %>% unite("group",c("population","controller"),sep="-")
kruskal.test(data=cleaned.data.ann, n ~ population)
pairwise.wilcox.test(cleaned.data.ann$n,factor(cleaned.data.ann$population),p.adj = "bonf")

kruskal.test(data=cleaned.data.ann, n ~ resources)
pairwise.wilcox.test(cleaned.data.ann$n,factor(cleaned.data.ann$resources),p.adj = "bonf")

kruskal.test(data=cleaned.data.ann, n ~ type)
pairwise.wilcox.test(cleaned.data.ann$n,factor(cleaned.data.ann$type),p.adj = "bonf")
write.csv(file="types.csv",pairwise.wilcox.test(cleaned.data.ann$n,factor(cleaned.data.ann$type),p.adj = "bonf")[["p.value"]])


boot.t.test(filter(cleaned.data.ann, type=="ABC")$n,filter(cleaned.data.ann, type=="ABCD")$n,R=500)

set.seed(500)
boot.t.test(filter(cleaned.data, population==350 & controller=="ANN")$n,
filter(cleaned.data, population==350 & controller=="Random")$n,R=500)

shapiro.test(filter(cleaned.data, population==350 & controller=="ANN")$n-filter(cleaned.data, population==350 & controller=="Random")$n)
hist(filter(cleaned.data, population==350 & controller=="ANN")$n-filter(cleaned.data, population==350 & controller=="Random")$n)


wilcox.test(filter(cleaned.data, controller=="ANN")$n,filter(cleaned.data, controller=="Random")$n)


cor.test(cleaned.data.ann$n, cleaned.data.ann$population, 
                    method = "spearman")

cor.test(cleaned.data.ann$n, cleaned.data.ann$population, 
                    method = "spearman")

cleaned.data %>% unite("group",c("type","controller","population","resources"),sep="-")


temp
cleaned.data %>%
    group_by(type,controller) %>%
   summarise(mean = mean(n), sd= sd(n),Q1 = quantile(n,0.25),median=median(n),Q3 = quantile(n,0.75), n=n())


head(inner_join(cleaned.data,cleaned.data,by = c("type"="type","controller"="controller")))


sum((df$ANN-mean(df$ANN))^2)
sum((df$Random-mean(df$Random))^2)

