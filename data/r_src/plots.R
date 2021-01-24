## Normality Tests ############

clust.df$size = clust.df$prop*clust.df$population

v = subset(df,type=="A")
shapiro.test(log(clust.df$prop*clust.df$population))

t$freq = t$freq/t$population
t$prop = t$prop/t$population



for (p in c(1:4)*500){
   print(kruskal.test(prop ~ factor(population) , data = subset(t,res==p)))
}




head(clust.df)

t = clust.df %>% unite(group, c("population","res","type"),sep="-",remove=FALSE)



# Proportions #######
hist(x)
x=log(clust.df$prop*clust.df$population)
ks.test(x=x, "pnorm", mean=mean(x), sd=sd(x))

library(dplyr)
prop.summary = clust.df %>%
  group_by(population,res,type) %>%
  summarise(mean = mean(prop), n = n(), sd = sd(prop), q1 = quantile(prop,0.25), median = median(prop),q3=quantile(prop,0.75) )


freq.summary = df %>%
  group_by(type) %>%
  summarise(mean = mean(freq), n = n(), sd = sd(freq), q1 = quantile(freq,0.25), median = median(freq),q3=quantile(freq,0.75) )

freq.summary




summary(data=clust.df, prop ~ factor(type))
aggregate(data=clust.df, prop ~ factor(type), median)
aggregate(data=clust.df, prop ~ factor(type), mean)


kruskal.test(data=clust.df, prop ~ factor(type))
kruskal.test(data=clust.df, prop ~ factor(population))
kruskal.test(data=clust.df, prop ~ factor(res))
####################

# Frequencies #######
head(df)
x=log(clust.df$prop*clust.df$population)
ks.test(x=x, "pnorm", mean=mean(x), sd=sd(x))

kruskal.test(data=df, freq ~ factor(type))
kruskal.test(data=df, freq ~ factor(population))
kruskal.test(data=df, freq ~ factor(res))


boxplot(data=df, freq ~ factor(res))

#####################


t = merge(count(clust.df,c("population","env","trial","type","res")),clust.df)
t$freq = 1/t$freq
t$prop = t$population * t$prop

hist(t$freq-t$prop)
ggplot(t, aes(x=population,y=freq-prop)) + 
stat_summary(fun=mean, geom="line",aes(group=1) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=1), size=2) 

head(xtabs(freq~type+population, df[c("type","population","freq")]))
chisq.test(t$prop,p=t$freq)

ks.test(x=clust.df$size, "pnorm", mean=mean(clust.df$size), sd=sd(clust.df$size))

hist(subset(clust.df,type=="ABCD")$prop)
head(xtabs(freq~type+population, df[c("type","population","freq")]))
chisq.test(xtabs(freq~type+population, df[c("type","population","freq")]))

## Regression for cluster number # 

require(AER)
require(performance)
require(MASS)
load("freq.RData")
load("cluster_data.RData")

load("boot_model.RData")

set.seed(123)
boot_data = dplyr::sample_n(freq.df,nrow(freq.df)*200,replace=TRUE)

smp_size <- floor(0.8 * nrow(boot_data))

train_ind <- sample(seq_len(nrow(boot_data)), size = smp_size)

train <- boot_data[train_ind, ]
test <- boot_data[-train_ind, ]

poisson.model <- glm(freq ~ population + res + type + controller, train, family = quasipoisson)

save(file="boot_model.RData",poisson.model)

summary(poisson.model)
mean(sqrt((predict(poisson.model,test,type="response")-test$freq)^2))

predict(poisson.model2,data.frame(population=25,res=10000,type="ABC",controller="ANN"),type="response")

dispersiontest(poisson.model)
poisson.model2 <- glm(freq ~ population + res + type + controller, freq.df, family = quasipoisson)

summary(poisson.model2)

poisson.model3  <- glm.nb(freq ~ population + res + type +controller,data= freq.df)
summary(poisson.model3)

compare_performance(poisson.model2,poisson.model3,rank=TRUE)
summary(poisson.model2)





#Regression for proportion within cluster#

cluster.data$freq=cluster.data$prop*cluster.data$population

outliers <- boxplot(cluster.data$freq,plot=FALSE)$out

poisson.model4 <- glm(freq ~ population +res+type, data=cluster.data[!(cluster.data$freq %in% outliers), ], family = quasipoisson)
summary(poisson.model4)


cluster.data.clean = cluster.data[!(cluster.data$freq %in% outliers), ]

cluster.data.clean$yhat = predict(poisson.model4,cluster.data.clean,type="response")



linear.model <-  lm(freq ~ population +res+type+controller, cluster.data)
summary(linear.model)
cluster.data$yhat = predict(linear.model,cluster.data.clean,type="response")
require(ggplot2)
ggplot(cluster.data.clean, aes(x=type,y=yhat)) +
stat_summary(fun=mean, geom="line",aes(group=1),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=1),alpha=0.5,size=2) +
   stat_summary(fun=mean, geom="line",aes(y=freq, group=2,colour="red"),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(y=freq,group=2,colour="red"),alpha=0.5,size=2) 

mean(abs((cluster.data$yhat-cluster.data$freq)))

compare_performance(linear.model,poisson.model4,rank=TRUE)


###################################


# More resources = more clusters
# ANN != ctrl
# More types = more clusters
# Bigger population = more clusters

### Create tables###############

hist(freq.df$freq)

require(expss)

head(cluster.data)

t= apply_labels(cluster.data,
                      prop = "Proportion",
                      mean = "Average Fitness",
                      population = "Population",
                      env = "Environment",
                      trial = "Repetition",
                      res = "Resources",
                      type = "Types of Resources",
                     controller = "Controller"

)


t %>% 
    tab_cells(prop) %>%
    tab_cols(total(), controller,type) %>% 
    tab_stat_mean_sd_n() %>%
    tab_last_sig_means(subtable_marks = "both") %>% 
    tab_pivot() %>% 
    set_caption("Table with summary statistics and significance marks.")

cro(t$type,t$controller)

###Bootstrapping #############

load("freq.RData")
load("cluster_data.RData")

require(nonpar)
require(boot)

a = freq.df[freq.df$controller=="ANN",]
colnames(a)[7] <- "ANN"
a=subset(a,select = -c(controller))

r = freq.df[freq.df$controller=="Random",]
colnames(r)[7] <- "Random"
r=subset(r,select = -c(controller))

df = merge(a,r)
rm(a,r)


##############################

df.pvals = data.frame()
set.seed(50)
for (pop in c(0.5,1:10,20)*50) {

for (resources in c(1:5)*500)
 {

   for (res.type in c("A","AB","ABC","ABCD") )
   {
      # t=data.frame(population = pop, res = res, type=type,pval = kruskal.test(data=subset(freq.df,population==pop&res==res), freq ~ factor(type))[["p.value"]])
      k = subset(df,population==pop&type==res.type&res==resources)
      r = boot.t.test(x=k$ANN,y=k$Random,R=500,paired=TRUE,alternative="greater")

      t=data.frame(population = pop,type=res.type,res=resources,
      diff.mean=mean(k$ANN-k$Random),
      pval = r[["boot.p.value"]])
      # conf.lwr=r[["boot.conf.int"]][["2.5%"]],
      # conf.upp=r[["boot.conf.int"]][["97.5%"]])


      df.pvals = rbind(df.pvals,t)
   }
}
}

df.pvals$conclusion=NA
df.pvals[df.pvals$pval<=0.11,]$conclusion = "Reject H0"
df.pvals[df.pvals$pval>0.11,]$conclusion = "Fail to reject H0"
df.pvals

table(df.pvals$conclusion)

subset(df.pvals,pval<0.1)

write.csv(df.pvals,"ANN_vs_Random_cluster_number_by_all.csv", row.names = FALSE)

df.pvals.pop = data.frame()
set.seed(50)
for (pop in c(0.5,1:10,20)*50) {
  
      k = subset(df,population==pop)
      r = boot.t.test(k$ANN,k$Random,R=500)

      t=data.frame(population = pop,
      diff.mean=mean(k$ANN-k$Random),
      pval = r[["boot.p.value"]],
      conf.lwr=r[["boot.conf.int"]][["2.5%"]],
      conf.upp=r[["boot.conf.int"]][["97.5%"]])


      df.pvals.pop = rbind(df.pvals.pop,t)
   
}
write.csv(df.pvals.pop,"ANN_vs_Random_cluster_number_by_population.csv", row.names = FALSE)

head(df.pvals.pop)

df.pvals.res = data.frame()
set.seed(50)
for (res in c(1:5)*500) {
  
      k = subset(df,resources==res)
      r = boot.t.test(k$ANN,k$Random,R=500)

      t=data.frame(resources=res ,
      diff.mean=mean(k$ANN-k$Random),
      pval = r[["boot.p.value"]],
      conf.lwr=r[["boot.conf.int"]][["2.5%"]],
      conf.upp=r[["boot.conf.int"]][["97.5%"]])


      df.pvals.res = rbind(df.pvals.res,t)
   
}

write.csv(df.pvals.res,"ANN_vs_Random_cluster_number_by_resources.csv", row.names = FALSE)

head(df.pvals.res)


df.pvals.type = data.frame()
set.seed(50)
for (res.type in c("A","AB","ABC","ABCD","ABCDE")) {
  
      k = subset(df,type==res.type)
      r = boot.t.test(k$ANN,k$Random,R=500)

      t=data.frame(type = res.type,
      diff.mean=mean(k$ANN-k$Random),
      pval = r[["boot.p.value"]],
      conf.lwr=r[["boot.conf.int"]][["2.5%"]],
      conf.upp=r[["boot.conf.int"]][["97.5%"]])


      df.pvals.type = rbind(df.pvals.type,t)
   
}
(df.pvals.type)
write.csv(df.pvals.type,"ANN_vs_Random_cluster_number_by_type.csv", row.names = FALSE)

summary(aov(data=cleaned.data, n ~ factor(type)*factor(controller)))



df.pvals.type

df.pvals.pop = data.frame()

for (pop in c(0.5,1:10)*50) {
  
   t=data.frame(population = pop,pval = kruskal.test(data=subset(freq.df,population==pop), freq ~ factor(type))[["p.value"]])
   df.pvals.pop = rbind(df.pvals.pop,t)
   
}

df.pvals.res = data.frame()

for (res in c(1:4)*500) {
  
   t=data.frame(res = res,pval = kruskal.test(data=subset(freq.df,res==res), freq ~ factor(type))[["p.value"]])
   df.pvals.res = rbind(df.pvals.res,t)
   
}


df.pvals.type

df.pvals.type.cmp = data.frame()
set.seed(300)
c=0
for (type1 in c("A","AB","ABC","ABCD")) {
  c= c+1
  for (type2 in c("A","AB","ABC","ABCD")[c:4]) {

      k = subset(df,type %in% c(type1,type2))
      r = boot.t.test(x=k$ANN,y=k$Random,R=500)

      t=data.frame(type.ANN = type1, type.ctrl = type2,
      diff.mean=mean(k$ANN)-mean(k$Random),
      pval = r[["boot.p.value"]],
      conf.lwr=r[["boot.conf.int"]][["2.5%"]],
      conf.upp=r[["boot.conf.int"]][["97.5%"]])

      df.pvals.type.cmp = rbind(df.pvals.type.cmp,t)
  }
   
}


df.pvals.type.cmp

write.csv(df.pvals.type.cmp,"ANN_vs_Random_cluster_number_by_pairwise_types.csv", row.names = FALSE)


df.pvals.pop.cmp = data.frame()
set.seed(100)
for (pop in c(0.5,1:10)*50) {

   k = subset(df,population==pop)
   r = boot.t.test(k$ANN,k$Random,R=500)
   t=data.frame(population = pop,
   diff.mean=mean(k$ANN)-mean(k$Random),
   pval = r[["boot.p.value"]],
   conf.lwr=r[["boot.conf.int"]][["2.5%"]],
   conf.upp=r[["boot.conf.int"]][["97.5%"]])

   df.pvals.pop.cmp = rbind(df.pvals.pop.cmp,t)
   
}

df.pvals.pop.cmp

write.csv(df.pvals.pop.cmp,"ANN_vs_Random_cluster_number_by_pairwise_populations.csv", row.names = FALSE)

df.pvals.pop = data.frame()

set.seed(100)
c=0
for (pop1 in c(0.5,1:10)*50) {
  c= c+1
  for (pop2 in (c(0.5,1:10)*50)[c:11] ) {

     if (pop1==pop2) next

      k = subset(df,population %in% c(pop1,pop2))
      r = boot.t.test(k[k$population==pop1,]$ANN,k[k$population==pop2,]$ANN,R=500)
      t=data.frame(pop.x = pop1, pop.y = pop2,
      diff.mean=mean(k[k$population==pop1,]$ANN)- mean(k[k$population==pop2,]$ANN),
      pval = r[["boot.p.value"]],
      conf.lwr=r[["boot.conf.int"]][["2.5%"]],
      conf.upp=r[["boot.conf.int"]][["97.5%"]])

      df.pvals.pop = rbind(df.pvals.pop,t)
  }
   
}


df.pvals.pop
head(df)
boxplot(freq.df$freq)
head(freq.df)
subset(cluster.data.ann,population==500)
aggregate(data=cluster.data,freq~population,mean)

tail(cluster.data.ann)
set.seed(500)
r = boot.t.test(df$ANN,df$Random,R=1000,paired=FALSE,"greater")


hist(aggregate(data=cluster.data.ann, prop ~ population + env + trial,median)$prop)

ggplot(cluster.data.ann, aes(x=factor(population),y=prop*population)) + geom_boxplot()


head(freq.df[freq.df$controller=="ANN",])
wilcox.test(df$ANN,df$Random,p.adjust.method = "bonf",paired=TRUE)


wilcox.test(cluster.data[cluster.data$controller=="ANN",]$prop, cluster.data[cluster.data$controller=="Random",]$prop,p.adjust.method = "bonf",paired=FALSE)


summary(cluster.data$prop*cluster.data$population)


ggplot(df,aes())
require(dplyr)
freq.df %>% group_by(env,controller) %>% mutate(cs = cumsum(freq))

aggregate(data=freq.df,freq ~ env + controller,sum)

mean(freq.df[freq.df$env>5 & freq.df$controller=="ANN",]$freq)
mean(freq.df[freq.df$env<=5 & freq.df$controller=="Random",]$freq)

g<-ggplot(freq.df, aes(y=scale(freq),x=population,colour=controller)) + 
stat_summary(fun=mean, geom="line",aes(group=controller),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller),alpha=0.5,size=2)
g

## Proportions of 

logit.model <- glm(larger ~ population + res + type , data = k, family = "binomial")

summary(logit.model)

chisq.test(table(k$population,k$larger),correct=FALSE) 
chisq.test(table(k$res,k$larger),correct=FALSE) 
chisq.test(table(k$type,k$larger),correct=FALSE) 
chisq.test(table(k$larger),correct=FALSE) 


summary(table(k$population,k$larger))

df$larger = df$ANN > df$Random

k = subset(df,ANN != Random)
head(k)

df <- subset(df, select = -c(larger))
df$larger = 0
df[df$ANN == df$Random,]$larger = "Same"
df[df$ANN > df$Random,]$larger = 1
df[df$ANN < df$Random,]$larger = 0

df[df$larger != "ANN",]$larger = "Random"


table(df$larger)/length(df$larger)

round(prop.table(df$larger,2)*100,digits=0)


wilcox.test(cluster.data[cluster.data$controller=="ANN",]$prop, cluster.data[cluster.data$controller=="Random",]$prop,p.adjust.method = "bonf",paired=FALSE)
wilcox.test(df$ANN,df$Random,p.adjust.method = "bonf",paired=TRUE)


g


g<-ggplot(cluster.data, aes(y=(prop),x=factor(type),colour=controller)) + 
stat_summary(fun=mean, geom="line",aes(group=controller),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller),alpha=0.5,size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0) +
       labs(x="Resource Types",y="Proportion",colour="Controller",fill="Controller",title=" Average Proportion of Population within Clusters") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("prop_v_type_by_controller.png",g)

nn <- ddply(cluster.data, "controller", transform, 
        mean  = mean(prop))

w = 2 * IQR((cluster.data$prop)) / length(cluster.data$prop)^(1/3)
w
g <- ggplot(cluster.data) +
  geom_histogram(aes(prop,y=..density..,fill=controller,color=controller),alpha=0.5,binwidth = w) + 
    geom_density(aes(prop,group=controller),adjust = 3)+
    geom_vline(aes(xintercept = mean),data=nn,linetype = 5) + facet_grid(controller~.) +
     labs(x="Proportion",y="Density",colour="Controller",fill="Controller",title=" Histogram of Proportion of Population within Clusters",caption="\nWhere the area under a density curve integrates to 1.") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g
ggsave("hist_prop_v_type_by_controller.png",g)


nn <- ddply(cluster.data, "controller", transform, 
        mean  = mean(log(prop)))

w = 2 * IQR(log(cluster.data$prop)) / length(cluster.data$prop)^(1/3)
g <- ggplot(cluster.data) +
  geom_histogram(aes(log(prop),y=..density..,fill=controller,color=controller),alpha=0.5,binwidth = w) + 
    geom_density(aes(log(prop),group=controller),adjust = 5)+
    geom_vline(aes(xintercept = mean),data=nn,linetype = 5) + facet_grid(controller~.) +
     labs(x="Proportion",y="Density",colour="Controller",fill="Controller",title=" Histogram of Proportion of Population within Clusters",caption="\nWhere the area under the density curve integrates to 1.") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g


g

g <- ggplot(cluster.data,aes(y=prop,x=factor(type))) +
 stat_summary(fun=median, geom="line", aes(group=1), size=0.8,alpha=0.7) +
  stat_summary(fun=median, geom="point", aes(group=1), size=1,alpha=0.7) +
   geom_ribbon(mapping = aes(y=(prop),x=factor(type),group=1),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median,colour=NA,alpha=0.1) +     labs(x="Clusters",y="Proportion",colour="Resource Types",fill="Resource Types",title=" Median Proportion of Population within Clusters") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

g <- ggplot(freq.df,aes(y=freq,x=factor(type),colour=controller)) +
 stat_summary(fun=mean, geom="line", aes(group=controller), size=0.8,alpha=0.7) +
  stat_summary(fun=mean, geom="point", aes(group=controller), size=1,alpha=0.7)+
    stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0) +
         labs(x="Resource Type",y="Cluster Number",colour="Controller",title=" Average Cluster Amount per\nResource Types in environment",caption="\nError Bars represent Standard Deviation.") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clusts_v_type_by_controller.png",g)

g <- ggplot(freq.df,aes(y=freq,x=factor(type))) +
 stat_summary(fun=median, geom="line", aes(group=1), size=0.8,alpha=0.7) +
  stat_summary(fun=median, geom="point", aes(group=1), size=1,alpha=0.7) +
   stat_summary(fun=mean, geom="line", aes(group=1,colour="red"), size=0.8,alpha=0.7) +
  stat_summary(fun=mean, geom="point", aes(group=1,colour="red"), size=1,alpha=0.7) +
   geom_ribbon(mapping = aes(y=(freq),x=factor(type),group=1),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median,colour=NA,alpha=0.1) +     labs(x="Clusters",y="Proportion",colour="Resource Types",fill="Resource Types",title=" Median Proportion of Population within Clusters") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

kruskal.test(data=freq.df, freq ~ factor(controller))[["p.value"]]
kruskal.test(data=df, freq ~ factor(population))[["p.value"]]
kruskal.test(data=df, freq ~ factor(res))[["p.value"]]


pairwise.wilcox.test(cluster.data$prop, cluster.data$controller,p.adjust.method = "bonf",paired=FALSE)[["p.value"]][1]
kruskal.test(data=cluster.data, prop ~ factor(type))[["p.value"]]
kruskal.test(data=cluster.data, prop ~ factor(population))[["p.value"]]
kruskal.test(data=cluster.data, prop ~ factor(res))[["p.value"]]

summary(aov(data=cluster.data, prop ~ factor(population) + factor(res) + factor(type) + factor(controller) ))

ggplot(cluster.data, aes(y=prop,x=factor(population))) + geom_boxplot()+
stat_summary(fun=mean, geom="line",aes(group=1,color="red"),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=1,color="red"), size=2) 


## Make Plots ################
{

require(plyr)
require(ggplot2)

title_size = 15
lab_size = 10

## Comparing Controllers ####

clust.df$controller = "ANN"
rand.clust.df$controller = "Random"

cluster.data = rbind(clust.df,rand.clust.df)

head(rand.clust.df)
head(cluster.data)

g <- ggplot(data=plyr::count(cluster.data,c("population","env","trial","type","res","controller")), aes(x=factor(population), y=freq,colour=controller),alpha=0.4) + 
 stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0,alpha=0.8) +
 labs(x="Population",y="Cluster Number",colour="Controller",fill="Controller",title="Average Amount of Clusters per\nResource Amount",caption="\nClusters were plotted using Levenshtein Similarity and K-Means where K was determined\nby the Silhouette method.\n\nError Bars represent Standard Deviation.") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusts_v_pop_by_controller.png",g)

g <- ggplot(data=plyr::count(cluster.data,c("population","env","trial","type","res","controller")), aes(x=factor(res), y=freq,colour=controller),alpha=0.4) + 
 stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller),width=0,alpha=0.8) +
 labs(x="Resources",y="Cluster Number",colour="Controller",fill="Controller",title="Average Amount of Clusters per\nResource Amount",caption="\nClusters were plotted using Levenshtein Similarity and K-Means where K was determined\nby the Silhouette method.\n\nError Bars represent Standard Deviation.") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusts_v_res_by_controller.png",g)


g<-ggplot(data=cluster.data,aes(y=mean/13,x=population,colour=controller)) +
stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "ribbon", fun.data = mean_se,aes(group=controller,fill=controller),alpha=0.2,colour=NA) +
    labs(x="Population",y="Fitness",colour="Controller",fill="Controller",title="Average Fitness per Population size") + 
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g
ggsave("fit_v_pop_by_controller.png",g)


g<-ggplot(data=cluster.data,aes(y=mean/13,x=factor(res),colour=controller)) +
stat_summary(fun=mean, geom="line",aes(group=controller) ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=controller), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller,colour=controller),width=0,alpha=0.8) +
    labs(x="Resources",y="Fitness",colour="Controller",fill="Controller",title="Average Fitness per Resource size",caption="\nError Bars represent Standard Deviation.") + 
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("fit_v_res_by_controller.png",g)

freq.df = plyr::count(cluster.data,c("population","env","trial","type","res","controller"))
w = 2 * IQR((freq.df$freq)) / length(freq.df$freq)^(1/3)
g<-ggplot(data=freq.df,aes(x=(freq),colour=controller,fill=controller)) +geom_histogram(binwidth=1,aes(y=..density..),alpha=0.3,position = 'identity')+
     labs(x="Cluster Number",y="Density",colour="Resource Types",fill="Resource Types",title="Histogram Showing Distribution of Language Clusters") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

nn <- ddply(freq.df, "controller", transform, 
        mean  = mean(freq))

# w = 2 * IQR((cluster.data$prop)) / length(cluster.data$prop)^(1/3)
g <- ggplot(freq.df) +
  geom_histogram(aes(freq,y=..density..,fill=controller,color=controller),alpha=0.5,binwidth = 1) + 
    geom_density(aes(freq,group=controller),adjust = 5)+
    geom_vline(aes(xintercept = mean),data=nn,linetype = 5) + facet_grid(controller~.) +
     labs(x="Cluster Number",y="Density",colour="Controller",fill="Controller",title=" Histogram of Proportion of Population within Clusters",caption="\nWhere the area under a density curve integrates to 1.") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g
ggsave("hist_freq_v_type_by_controller.png",g)




# w = 2 * IQR((cluster.data$prop)) / length(cluster.data$prop)^(1/3)
# g <- ggplot(cluster.data,aes(x=(prop),fill=controller)) +geom_histogram(binwidth=w,alpha=0.4,position = 'identity')+
#      labs(x="Proportion",y="Frequency",colour="Resource Types",fill="Resource Types",title=" Histogram Showing Proportion of Agents\nwithin Each Cluster") +
#       theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
# g


g <- ggplot(freq.df,aes(y=freq,x=controller)) + 
 stat_summary(fun=mean, geom="line", aes(group=1), size=0.8,alpha=0.7) +
  stat_summary(fun=mean, geom="point", aes(group=1), size=1,alpha=0.7) +
  stat_summary(geom = "ribbon", fun.data = mean_se,aes(group=1),alpha=0.2) +
     labs(x="Controller",y="Average Clusters",colour="Controller",fill="Controller",title=" Average Amount of Clusters by Controller",caption="\nShaded Area represent Standard Deviation.") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clust_v_controller.png",g)

g <- ggplot(cluster.data,aes(y=prop,x=controller)) + 
 stat_summary(fun=mean, geom="line", aes(group=1), size=0.8,alpha=0.7) +
  stat_summary(fun=mean, geom="point", aes(group=1), size=1,alpha=0.7) +
  stat_summary(geom = "ribbon", fun.data = mean_se,aes(group=1),alpha=0.2) +
     labs(x="Controller",y="Average Proportion",colour="Controller",fill="Controller",title=" Average Proportion of Population within\neach Cluster by Controller",caption="\nShaded Area represent Standard Deviation.") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("prop_v_controller.png",g)


g <- ggplot(freq.df,aes(y=freq,x=controller)) + geom_boxplot() +
stat_summary(fun=mean, geom="line",aes(group=1,colour="red") ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=1,colour="red"), size=2) +
    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(group=1,fill="red"),colour=NA,alpha=0.2) +
        labs(x="Resource Types",y="Cluster Number",title="Boxplots of Cluster Number per\nResource Types in environment") + 
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5),legend.position = "none")
g



#########################################################

{
g <- ggplot(data=plyr::count(clust.df,c("population","env","trial","type","res")), aes(x=factor(population), y=freq),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(group=1), size=0.8) +
   stat_summary(fun=mean, geom="point", aes(group=1), size=2) +

    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(group=1),alpha=0.2) +
 labs(x="Population",y="Cluster Number",color="Resource Types",title="Average Amount of Clusters per\nPopulation size with 95% CI",caption="\nClusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g



df = (merge(aggregate(data=clust.df,mean~population + env + trial + type+res,mean),plyr::count(clust.df,c("population","env","trial","type","res"))))

g <- ggplot(df, aes(x=type,y=freq)) + geom_boxplot() +
stat_summary(fun=mean, geom="line",aes(group=1,colour="red") ,size=0.8) +
   stat_summary(fun=mean, geom="point",aes(group=1,colour="red"), size=2) +
    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(group=1,fill="red"),colour=NA,alpha=0.2) +
        labs(x="Resource Types",y="Cluster Number",title="Boxplots of Cluster Number per\nResource Types in environment") + 
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5),legend.position = "none")
g
ggsave("box_clust_v_types_mean.png",g)


g <- ggplot(data=plyr::count(clust.df,c("population","env","trial","type","res")), aes(x=factor(population), y=freq),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(group=1), size=0.8) +
   stat_summary(fun=mean, geom="point", aes(group=1), size=2) +

    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(group=1),alpha=0.2) +
 labs(x="Population",y="Cluster Number",color="Resource Types",title="Average Amount of Clusters per\nPopulation size with 95% CI",caption="\nClusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusters_v_population.png",g)


g <- ggplot(data=plyr::count(clust.df,c("population","env","trial","type","res")), aes(x=factor(res), y=freq),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(group=1), size=0.8) +
  stat_summary(fun=mean, geom="point", aes(group=1), size=2) +

    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(group=1),alpha=0.2) +
 labs(x="Resources",y="Cluster Number",color="Resource Types",title="Average Amount of Clusters per\nResource amount size with 95% CI",caption="\nClusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusters_v_res.png",g)


g <- ggplot(data=plyr::count(clust.df,c("population","env","trial","type","res")), aes(x=population, y=freq,colour=type),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(group=type), size=0.8) +
  stat_summary(fun=mean, geom="point", aes(group=type), size=2) +

    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(fill=type),alpha=0.2,colour=NA) +
 labs(x="Population",y="Cluster Number",fill="Resource Types",color="Resource Types",title="Average Amount of Clusters per\nResource amount size with 95% CI",caption="\nClusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusters_v_pop_by_type.png",g)


g <- ggplot(data=plyr::count(clust.df,c("population","env","trial","type","res")), aes(x=res,y=freq,group=type),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(colour=type), size=0.8) +
  stat_summary(fun=mean, geom="point", aes(colour=type), size=2) +

    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(fill=type,colour=NA),alpha=0.2,colour=NA) +
 labs(x="Resources",y="Cluster Number",colour="Resource Types",fill="Resource Types",title="Average Amount of Clusters per\nResource Amount with 95% CI",caption="\nClusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusters_v_res_by_type.png",g)

g <- ggplot(data=plyr::count(clust.df,c("population","env","trial","type","res")), aes(x=population,y=freq,group=factor(res)),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(colour=factor(res)), size=0.8) +
  stat_summary(fun=mean, geom="point", aes(colour=factor(res)), size=2) +
    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(fill=factor(res),colour=NA),alpha=0.2,colour=NA) +
 labs(x="Population",y="Cluster Number",colour="Resource Types",fill="Resource Types",title="Average Amount of Clusters per\nPopulation size Amount with 95% CI",caption="\nClusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g
ggsave("clusters_v_pop_by_res.png",g)

g<-ggplot(data=df,aes(y=mean/13,x=freq)) +
  stat_summary(fun=mean, geom="line", size=1) +
  stat_summary(fun=mean, geom="point", size=2) +
    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",alpha=0.2) +
     labs(x="Cluster Number",y="Fitness",colour="Resource Types",fill="Resource Types",title="Average Cluster Amount vs Average Fitness") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g
ggsave("clusters_v_fit.png",g)


g<-ggplot(data=df,aes(y=mean/13,x=freq,colour=type,fill=type)) +
  stat_summary(fun=mean, geom="line", size=1) +
  stat_summary(fun=mean, geom="point", size=2) +
    stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",alpha=0.15 ,colour=NA) +
     labs(x="Cluster Number",y="Fitness",colour="Resource Types",fill="Resource Types",title="Average Cluster Amount vs Average Fitness") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("clusters_v_fit_by_type.png",g)

g <- ggplot(data=clust.df, aes(x=population, y=mean/13,group=factor(res))) + 
 stat_summary(fun=mean, geom="line", aes(colour=factor(res)), size=1,alpha=0.7) +
  stat_summary(fun=mean, geom="point", aes(colour=factor(res)), size=1,alpha=0.7) +
      stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(fill=factor(res)),alpha=0.15) +
 labs(x="Population",y="Fitness",colour="Resource Amounts",fill="Resource Amounts",title="Average Fitness per\nPopulation size with 95% CI") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("fitness_v_population_by_res.png",g)


g <- ggplot(data=clust.df, aes(x=population, y=(mean/13),group=type)) + 
 stat_summary(fun=mean, geom="line", aes(colour=type), size=0.8,alpha=0.7) +
  stat_summary(fun=mean, geom="point", aes(colour=type), size=1,alpha=0.7) +
      stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(fill=type),alpha=0.15) +
 labs(x="Population",y="Fitness",colour="Resource Types",fill="Resource Types",title="Average Fitness per\nPopulation size with 95% CI") + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("fitness_v_population_by_type.png",g)


w = 2 * IQR((df$freq)) / length(df$freq)^(1/3)
g<-ggplot(data=df,aes(x=(freq),colour=type,fill=type)) +geom_histogram(binwidth=1,aes(y=..density..),alpha=0.3,position = 'identity')+
     labs(x="Cluster Number",y="Density",colour="Resource Types",fill="Resource Types",title="Histogram Showing Distribution of Language Clusters") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g
ggsave("hist_clusters.png",g)

w = 2 * IQR((df$mean)) / length(df$freq)^(1/3)
g<-ggplot(data=df,aes(x=(mean),fill=type)) +geom_histogram(binwidth=w,aes(y=..density..),alpha=0.5,position = 'identity')+
     labs(x="Fitness",y="Density",fill="Resource Types",title="Histogram Showing Distribution of Agent Fitness") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g
ggsave("hist_fitness.png",g)

w = 2 * IQR(log(df$mean)) / length(df$freq)^(1/3)
g<-ggplot(data=df,aes(x=log(mean),colour=type,fill=type)) +geom_histogram(binwidth=w,aes(y=..density..),alpha=0.3,position = 'identity')+
     labs(x="Fitness",y="Density",colour="Resource Types",fill="Resource Types",title="Histogram Showing Distribution of Agent ln(Fitness)") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g
ggsave("hist_fitness_log.png",g)

w = 2 * IQR((clust.df$prop)) / length(clust.df$prop)^(1/3)
g <- ggplot(clust.df,aes(x=(prop),fill=type)) +geom_histogram(binwidth=w,alpha=0.4,position = 'identity')+
     labs(x="Proportion",y="Frequency",colour="Resource Types",fill="Resource Types",title=" Histogram Showing Proportion of Agents\nwithin Each Cluster") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))

g
ggsave("hist_proportion.png",g)


g <- ggplot(clust.df,aes(y=(prop),x=type,fill=type)) +geom_boxplot(alpha=0.4,position = 'identity',outlier.alpha = 0)+
     labs(x="",y="Proportion",colour="Resource Types",fill="Resource Types",title=" Boxplots Showing Proportion of Agents\nwithin Each Cluster") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("box_proportion.png",g)


g <- ggplot(clust.df,aes(y=prop,x=factor(population),fill=type)) +geom_boxplot(alpha=0.4, outlier.alpha = 0)+
     labs(x="Population",y="Proportion",colour="Resource Types",fill="Resource Types",title=" Boxplots Showing Proportion of Agents within each\ncluster by Population size") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 

g
ggsave("box_proportion_by_pop.png",g)


g <- ggplot(clust.df,aes(y=prop,x=factor(res),fill=type)) +geom_boxplot(alpha=0.4, outlier.alpha = 0)+
     labs(x="Resources",y="Proportion",colour="Resource Types",fill="Resource Types",title=" Boxplots Showing Proportion of Agents within each\ncluster by Resource amount") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g
ggsave("box_proportion_by_res.png",g)


w = 2 * IQR(log(clust.df$prop)) / length(clust.df$prop)^(1/3)
g <- ggplot(clust.df,aes(x=log(prop),fill=type)) +geom_histogram(binwidth=w,aes(y=..density..),alpha=0.3,position = 'identity')+
     labs(x="Proportion",y="Density",colour="Resource Types",fill="Resource Types",title=" Histogram Showing ln(Proportion of Agents)\nwithin Each Cluster") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g
ggsave("hist_proportion_log.png",g)



}

g <- ggplot(df,aes(y=freq,x=factor(type))) + 
 stat_summary(fun=mean, geom="line", aes(group=1), size=0.8,alpha=0.7) +
  stat_summary(fun=mean, geom="point", aes(group=1), size=1,alpha=0.7) +
      stat_summary(fun.data = "mean_cl_normal", geom = "ribbon",aes(group=1),alpha=0.15) +
     labs(x="Resource Types",y="Average Clusters",colour="Resource Types",fill="Resource Types",title=" Average Amount of Clusters by\nAmount of Resource Types") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("clust_v_types.png",g)

g <- ggplot(clust.df,aes(y=prop,x=population,colour=type)) +
geom_ribbon(mapping = aes(y=(prop),x=population,fill=type),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median,colour=NA,alpha=0.2) +
     labs(x="",y="Proportion",colour="Resource Types",fill="Resource Types",title=" Proportion of Agents\nwithin Each Cluster") +
      stat_summary(fun=median, geom="line", aes(colour=type), size=0.8,alpha=0.7) +
         stat_summary(fun=median, geom="point", aes(colour=type), size=1,alpha=0.7) +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

g <- ggplot(merge(plyr::count(clust.df,c("population","env","trial","type","res")),clust.df),aes(y=prop,x=freq,colour=type)) +
 stat_summary(fun=median, geom="line", aes(colour=type), size=0.8,alpha=0.7) +
  stat_summary(fun=median, geom="point", aes(colour=type), size=1,alpha=0.7) +
   geom_ribbon(mapping = aes(y=(prop),x=freq,fill=type),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median,colour=NA,alpha=0.1) +     labs(x="Clusters",y="Proportion",colour="Resource Types",fill="Resource Types",title=" Median Proportion of Population within Clusters") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("prop_v_clusters_by_type.png",g)

g <- ggplot(merge(plyr::count(clust.df,c("population","env","trial","type","res")),clust.df),aes(y=prop,x=freq)) +
 stat_summary(fun=median, geom="line", aes(group=1), size=0.8,alpha=0.7) +
  stat_summary(fun=median, geom="point", aes(group=1), size=1,alpha=0.7) +
   geom_ribbon(mapping = aes(y=(prop),x=freq,group=1),
                  stat = "summary",
                  fun.min = function(z) {quantile(z,0.25)},
                  fun.max = function(z) {quantile(z,0.75)},
                  fun = median,colour=NA,alpha=0.1) +     labs(x="Clusters",y="Proportion",colour="Resource Types",fill="Resource Types",title=" Median Proportion of Population within Clusters") +
      theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g
ggsave("prop_v_clusters.png",g)


# w = 2 * IQR(log(df$mean/max(df$mean))) / length(df$freq)^(1/3)
# g<-ggplot(data=df,aes(x=log(mean/max(mean)),fill=factor(population))) +geom_histogram(binwidth=w,alpha=0.8)+
#      labs(x="Fitness",y="Frequency",colour="Resource Types",fill="Resource Types",title="Histogram Showing Distribution of Agent ln(Fitness)") +
#       theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
# g

# ggsave("hist_fitness_by_pop.png",g)

# w = 2 * IQR((df$mean/max(df$mean))) / length(df$freq)^(1/3)
# g<-ggplot(data=df,aes(x=(mean/max(mean)),fill=factor(res))) +geom_histogram(binwidth=w,alpha=0.8)+
#      labs(x="Fitness",y="Frequency",colour="Resource Amount",fill="Resource Amount",title="Histogram Showing Distribution of Agent ln(Fitness)") +
#       theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
# g

}

###########################################################