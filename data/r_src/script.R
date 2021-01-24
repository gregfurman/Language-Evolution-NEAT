library(ggplot2)
library(ggrepel)
library(jsonlite)
library(ggfortify)
library(MASS)
library(cluster)
library(ggpubr)
library(gridExtra)
library(pracma)
library(dplyr)
library(plyr)
library(NbClust)
library(factoextra)
library(FactoMineR)
library(plotly)
library(ca)
library(tidyverse)

neat = read.csv("fitness_25.csv")

for (i in c(0.5,1:10)){
   neat = rbind(neat,read.csv(paste("fitness_",i*50,".csv",sep="")))

}

neat = rbind(neat,read.csv("fitness_500.csv"))

plot.fitness.single(neat)
neat$resources = factor(neat$resources)
neat$upper = neat$average/neat$agents + qt(0.975,df=4999)* (sqrt(neat$MSB+neat$MSW)/neat$agent) / sqrt(5000) # https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/consmean.htm (grand mean CI)
neat$lower = neat$average/neat$agents + qt(0.025,df=4999)* (sqrt(neat$MSB+neat$MSW)/neat$agent) / sqrt(5000)

g = ggplot(data=neat,aes(colour=factor(resources),x=Generation,y=average/agents)) + geom_line()+
geom_ribbon(aes(x=Generation,ymin=lower,ymax=upper,fill=factor(resources)),alpha=0.2,colour=NA) +
   ggtitle(paste("Graph showing average fitness of Talking Agents over evolutionary time.\nParameters: {Talking agents per simulation = ",unique(neat$agents)[1],", NEAT population = 500}",sep=""))+
      labs(y="Average Fitness",fill="Resources",colour="Resources")+
         theme(plot.title = element_text(hjust = 0.5),text = element_text(size=15))

g

plot.fitness.single(neat)

plot.fitness.single= function(neat) {

   scale = max(abs(rnorm(10000, mean=4, sd=2)))

   g = ggplot(data=neat,aes(colour=factor(resources),x=Generation,y=average/scale)) + geom_line()+
   geom_ribbon(aes(x=Generation,ymin=lower/scale,ymax=upper/scale,fill=factor(resources)),alpha=0.4,colour=NA) +
      ggtitle(paste("Normalised average fitness per Talking Agents over evolutionary time.\n{Talking agents per simulation = ",unique(neat$agents)[1],"}",sep=""))+
         labs(y="Average Fitness",fill="Resources",colour="Resources")+
            theme(plot.title = element_text(hjust = 0.5),text = element_text(size=15))
   
   return(g)

}


ggsave("norm_neat.png",plot.fitness.all.grid(neat))

plot.fitness.all.grid= function(neat) {

   vec <- list()
   count=1

   neat$average = neat$average/neat$agents
   neat$upper = neat$average + qt(0.975,df=499)* (sqrt(neat$MSB)/neat$agents / sqrt(500)) # https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/consmean.htm (grand mean CI)
   neat$lower = neat$average + qt(0.025,df=499)* (sqrt(neat$MSB)/neat$agents / sqrt(500))

   scaleFUN <- function(x) sprintf("%.3f", x)

   for (agent in unique(neat$agents)){

      net = subset(neat,agents==agent)
      g= plot.fitness.single(net)
      g = g+ theme(plot.title = element_text(hjust = 0.5,size=10),text = element_text(size=8)) + scale_y_continuous(labels=scaleFUN)
      vec[[count]] = g
      count = count+1

   }

   n <- length(vec)
   nCol <- floor(sqrt(n))

   return(do.call("grid.arrange", c(vec, ncol=nCol)))

}

plot.fitness.all.grid.resources = function(neat){

   vec <- list()
   count=1

   # neat$average = (neat$average) / (qnorm(0.99,mean=4,sd=2)*neat$resources)
   # neat$MSB = (neat$MSB) / (qnorm(0.99,mean=4,sd=2)*neat$resources)^2

   neat$upper = (neat$average + qt(0.975,df=499)* (sqrt(neat$MSB)) / sqrt(500)) # https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/consmean.htm (grand mean CI)
   neat$lower = (neat$average + qt(0.025,df=499)* (sqrt(neat$MSB)) / sqrt(500))

   for (resource in unique(neat$resources)){

      net = subset(neat,resources==resource)
      g= plot.fitness.single.resources(net)
      g = g+ theme(plot.title = element_text(hjust = 0.5,size=10),text = element_text(size=8))
      vec[[count]] = g
      count = count+1

   }

   n <- length(vec)
   nCol <- floor(sqrt(n))
   return(do.call("grid.arrange", c(vec, ncol=nCol)))

}

plot.fitness.all.grid(neat)

ggsave("NEAT_FIGURES.png",plot.fitness.all.grid(neat))


plot.fitness.all = function(neat){

   neat$resources = factor(neat$resources)
   neat$upper = neat$average/neat$agents + qt(0.975,df=499)* (sqrt(neat$MSB)/neat$agent) / sqrt(500) # https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/consmean.htm (grand mean CI)
   neat$lower = neat$average/neat$agents + qt(0.025,df=499)* (sqrt(neat$MSB)/neat$agent) / sqrt(500)
   
   g = ggplot(data=neat,aes(colour=interaction(factor(resources),factor(agents)),x=Generation,y=average/agents)) + geom_line()+
   geom_ribbon(aes(x=Generation,ymin=lower,ymax=upper,fill=interaction(factor(resources)),alpha=0.05,colour=factor(agents))) +
      ggtitle(paste("Average fitness of Talking Agents over evolutionary time.\n{Talking agents per simulation = ",unique(neat$agents)[1],", NEAT population = 500}",sep=""))+
         labs(y="Average Fitness",fill="Resources",colour="Resources")+
            theme(plot.title = element_text(hjust = 0.5,size=10),text = element_text(size=8))
   
   return(g)


}

fitness.cor.test = function(neat){

   df = data.frame()


   for (agent in unique(neat$agents)){

      for (resource in unique(neat$resources)){

         net = subset(neat,agents==agent & resources==resource)
         res = cor.test(y=net$average,x=net$Generation,method="spearman")
         row = data.frame(agents=agent,resources=resource,rho=res$estimate[[1]],p.value=format(round(res$p.value[[1]], 5), nsmall = 2))
         df = rbind(df,row)
      }

   }

      return(df)

}

fitness.cor.test(neat)
plot.fitness.all(neat)

# ************************************************************************************************************

df =read.csv("networkvrandom.txt")[2:7]

fitness.t.test = function(agents,size){

   df = data.frame()

   # for (agent in unique(neat$agents))
   {

      for (resource in unique(agents$resources)){

         # net = subset(neat,agents==agent & resources==resource)
         net = subset(agents,resources==resource)


         res = t.test(x=agents[agents$resources==resource & agents$hashcode,]$fitness,y=agents[agents$resources==resource & !agents$hashcode,]$fitness,alternative = "greater", var.equal = FALSE)
         row = data.frame(agents=size,resources=resource,t.stat=res$statistic[[1]],p.value=format(round(res$p.value[[1]], 5), nsmall = 2),mean.network=res$estimate[[1]],mean.random=res$estimate[[2]])
         df = rbind(df,row)
      }

      }

      return(df)

}

df.means = (rbind(cbind(agents=df$agents,resources=df$resources,stack(df[5])),cbind(agents=df$agents,resources=df$resources,stack(df[6]))))
colnames(df.means) = c("agents","resources","mean","Controller")
ggplot(data=df, aes(y=mean.network,x=factor(agents))) + geom_boxplot()

g=ggplot(data=df.means, aes(y=mean,x=Controller,colour=Controller)) +labs(y="Average Fitness",x="Group") +geom_boxplot() +
            ggtitle("Boxplots of agent fitness with an evolved controller for\nbidding versus random bid values of all post-evolution tests")+
                  theme(plot.title = element_text(hjust = 0.3),text = element_text(size=15))
g
ggsave("random_v_network.png",g)

model.anova = aov(data=df.means, mean ~ factor(Controller) + factor(Controller)*factor(agents) + factor(Controller)*factor(resources) )
summary(model.anova)
TUKEY <- TukeyHSD(x=model.anova,conf.level=0.95)




{
A = agent.total.stats(c("A"))
AB = agent.total.stats(c("A","B"))
ABC = agent.total.stats(c("A","B","C"))
ABCD = agent.total.stats(c("A","B","C","D"))
}
cols = c("fitness","population","type","environment","resources","hashcode")

total = rbind(AB[cols],ABC[cols],ABCD[cols])
total[total$hashcode == TRUE,]$hashcode = "ANN"
total[total$hashcode == FALSE,]$hashcode = "Random"

g<-ggplot(aggregate(data=total, fitness ~ resources + population + hashcode + type,mean), aes(y=fitness,x=hashcode,colour=type)) + geom_boxplot()
g + xlab("Controller")
ggsave("ANNvsRandom.png",g+ xlab("Controller"))

{
sim_df_A = create.similarity.df(c("A"),ANN=TRUE)
sim_df_AB = create.similarity.df(c("A","B"),ANN=TRUE)
sim_df_ABC = create.similarity.df(c("A","B","C"),ANN=TRUE)
sim_df_ABCD = create.similarity.df(c("A","B","C","D"),ANN=TRUE)


sim_df = rbind(sim_df_A,sim_df_AB,sim_df_ABC,sim_df_ABCD)
save(sim_df,file="ann_sim_df.RData")

}


save(rand_sim_df,file="rand_sim_df.RData")

head(rand_sim_df_ABCD)

load("rand_sim_df.RData")

plot(x=rand_sim_df_AB$agents,y=rand_sim_df_AB$mean/rand_sim_df_AB$sd)


rand_sim_df = rbind(rand_sim_df_A,rand_sim_df_ABC,rand_sim_df_AB,rand_sim_df_ABCD)

head(rand_sim_df)

save(sim_df, file = "sim_df.RData")
load("sim_df.RData") 

library(ggplot2)
library(foreach)
library(doParallel)

registerDoParallel(4)
terms = c("A","B","C","D") 
foreach (i=1:4,.combine=rbind) %dopar% {
  cluster.data.frame(terms[1:i],controller=TRUE)
}

ABCD_ANN = cluster.data.frame(terms,controller=TRUE)

{
A_ANN = cluster.data.frame(c("A"),controller=TRUE)
save(A_ANN,file="A_ANN.RData")

AB_ANN = cluster.data.frame(c("A","B"),controller=TRUE)
save(AB_ANN,file="AB_ANN.RData")

ABC_ANN = cluster.data.frame(c("A","B","C"),controller=TRUE)
save(ABC_ANN,file="ABC_ANN.RData")

ABCD_ANN = cluster.data.frame(c("A","B","C","D"),controller=TRUE)
save(ABCD_ANN,file="ABCD_ANN.RData")

ABCDE_ANN = cluster.data.frame(c("A","B","C","D","E"),controller=TRUE)
save(ABCDE_ANN,file="ABCDE_ANN.RData")

}

head(ABCDE_ANN)

{
A_ctrl = cluster.data.frame(c("A"),controller=FALSE)
save(A_ctrl,file="A_ctrl.RData")

AB_ctrl = cluster.data.frame(c("A","B"),controller=FALSE)
save(AB_ctrl,file="AB_ctrl.RData")

ABC_ctrl = cluster.data.frame(c("A","B","C"),controller=FALSE)
save(ABC_ctrl,file="ABC_ctrl.RData")

ABCD_ctrl = cluster.data.frame(c("A","B","C","D"),controller=FALSE)
save(ABCD_ctrl,file="ABCD_ctrl.RData")

ABCDE_ctrl = cluster.data.frame(c("A","B","C","D","E"),controller=FALSE)
save(ABCDE_ctrl,file="ABCDE_ctrl.RData")
}

{
ABCDE_ANN = cluster.data.frame(c("A","B","C","D","E"),controller=TRUE)
save(ABCDE_ANN,file="ABCDE_ANN.RData")
}

head(ABCD_ANN)

load("ABCDE_ctrl.RData")
load("ABCDE_ANN.RData")
load("ABCD_ANN.RData")
load("ABC_ANN.RData")
load("AB_ANN.RData")
load("A_ANN.RData")
load("ABCD_ctrl.RData")
load("ABC_ctrl.RData")
load("AB_ctrl.RData")
load("A_ctrl.RData")


agent_data = dplyr::bind_rows(ABCDE_ANN,ABCDE_ctrl,ABCD_ANN,ABC_ANN,AB_ANN,A_ANN,ABCD_ctrl,ABC_ctrl,AB_ctrl,A_ctrl)

agent_data$cluster = agent_data$cluster +1


agent_data = agent_data %>% 
  rename(
    controller = hashcode
  )

save(agent_data,file="agent_data.RData")


load("agent_data.RData")
agent_data[agent_data$controller==TRUE,]$controller = "ANN"
agent_data[agent_data$controller!="ANN",]$controller = "Random"

save(agent_data,file="agent_data.RData")


load("cluster_data.RData")

k= filter(agent_data,environment==0 & resources==500 & controller=="ANN" & population==500 & type=="ABCD")

cluster_data = agent_data %>% count(resources,environment, population,type,controller,trial,cluster) %>% 
# dplyr::select(c(resources, population,type,controller,cluster,n)) %>% 
group_by(resources, population,type,controller) %>%
summarise(largest=max(n),SS= sum( (n - mean(n) )^2 ),mean=mean(n), sd=sd(n),IQR = IQR(n), count = n())



library(dplyr)
library(rstatix)
library(ggplot2)
k=agent_data %>% count(resources,environment, population,type,controller,trial,cluster)


1 – (1800/2000)2 – (200/2000)2 = 0.18

k$prop = (k$n/k$population)^2
k$types = nchar(k$type)
k = k %>% select(-c("LVD"))
tail(k)
df = k %>% group_by(resources,population,controller,types,environment,trial) %>% summarise(lvd= 1-sum(prop)) #%>%
ggplot(df,aes(y=lvd,x=population,colour=factor(controller))) + geom_point() + geom_line(aes(y=yhat))
df = cbind(population,lvd,controller=rep("ANN",1025/25))
head(df)
library(rms)
install.packages("rms")


df$yhat = predict(mylogit, df,type="response")

mylogit <- glm(lvd ~ resources + population + types + controller, data = k %>% group_by(resources,population,controller,types,environment,trial) %>% summarise(lvd= 1-sum(prop)) 
, family = "binomial")

pR2 = 1 - mylogit$deviance / mylogit$null.deviance
pR2

rep(0,5)
population=seq(0,1000,25)
lvd=predict(mylogit,list(population=pop,resources=rep(0,1025/25), types=rep(0,1025/25), controller=rep("ANN",1025/25)),type="response")
length(rep(0,1000/25)
)
head(yhat)


plot(mtcars$wt, mtcars$vs, pch = 16, xlab = "WEIGHT (g)", ylab = "VS")

plot(mylogit)
summary(mylogit)

1-sum((k$n/k$population)^2)


save(k,file="count_data.RData")
load("count_data.RData")

load("neg_bin.RData")
summary(lm(data=k,n ~ controller + population + resources))


k %>% 
group_by(population,controller) %>%
summarise(den=var(n)/n(),mean=mean(n),n=n()) %>%
group_by(population) %>%
summarise(diff.mean = diff(mean),pvalue=2*pt(q=diff.mean/sqrt(sum(den)), df=sum(n), lower.tail=FALSE)) #%>%


k %>% 
group_by(resources,controller) %>%
summarise(den=var(n)/n(),mean=mean(n),n=n()) %>%
group_by(resources) %>%
summarise(diff.mean = diff(mean),pvalue=2*pt(q=diff.mean/sqrt(sum(den)), df=sum(n), lower.tail=FALSE)) #%>%

k %>% 
group_by(type,controller) %>%
summarise(den=var(n)/n(),mean=mean(n),n=n()) %>%
group_by(type) %>%
summarise(diff.mean = diff(mean),pvalue=2*pt(q=diff.mean/sqrt(sum(den)), df=sum(n), lower.tail=FALSE),error=  qt((0.95 + 1)/2, df = sum(n) - 1) * sqrt(sum(den)) / sqrt(sum(n)))
) #%>%


hist(k$n)

library(tidyverse) 
library(caret) 
install.packages("caret")

library(stargazer)
library(MASS)
library(dplyr)

sample_n(k,100)

load("count_data.RData")

df1 = k %>%
   group_by(controller,population,type,resources) %>%
   filter(!(abs(n - median(n)) > 2*sd(n)))





df1

library(tidyverse)




negative.binomial.model <-glm.nb(n ~ controller + population , data = df1 %>% ungroup() %>% sample_n(100000))

summary(negative.binomial.model)

negative.binomial.model <-glm.nb(n ~ population , data =k)

set.seed(5000)
negative.binomial.model <-glm.nb(n ~ controller*(population +resources + types) , data =df1)


stargazer(negative.binomial.model,type="text")
save(negative.binomial.model,file="neg_bin.RData")


summary(negative.binomial.model)
library(AER)
odTest(negative.binomial.model)
poisson.model <- glm(n ~ controller + population +resources + types, family=poisson(link="log"), data = k)
negative.binomial.model <-glm.nb(n ~ controller*(population +resources + types) , data = k)
anova(negative.binomial.model)

exp(negative.binomial.model$coefficients)

df1$types = nchar(df1$type)
k.means = k %>% group_by(controller,resources,population,types) %>% summarise(n=mean(n))
k.means$phat= predict(negative.binomial.model, k.means, type = "response")
 round(mean(abs(k.means$n-k.means$phat)),4)
predict(negative.binomial.model, data.frame(controller="ANN",population=0,resources=0,types=5), type = "response")

save(poisson.model,file="neg_pois.RData")
load("neg_pois.RData")
summary(poisson.model)

stargazer(negative.binomial.model,type="html",
dep.var.labels="Agents Within Language Cluster",
covariate.labels=c("No Controller","Population Size","Resource Amount","No. of Resource Types","No Controller X Population Size","No Controller X Resource Amount","No Controller X Resource Types"),ci=TRUE, ci.level=0.90, single.row=TRUE,
summary=FALSE,add.lines = list(c("Mean Absolute Error", round(mean(abs(k.means$n-k.means$phat)),4)),c("Chisq goodness-of-fit",pchisq(negative.binomial.model$deviance, negative.binomial.model$df.residual, lower.tail=FALSE))))


exp(negative.binomial.model$coefficients)


exp(poisson.model2$coefficients)


(est <- cbind(Estimate = coef(poisson.model2), confint(poisson.model2)))
k$types = nchar(k$type)
k$yhat <- predict(poisson.model2, k, type = "response")

mean(subset(k, population==200 &resources==500&types==3)$n)
plot(x=k$n,y=k$yhat)
plot(poisson.model2)
model <- lm(n ~ log(population) ,k)
summary(model)


plot(y=k$n/k$population,k$population)
hist(k$n)
k %>% 
group_by(population,controller,resources,type) %>%
summarise(var=mean(n)) %>% 
ggplot(aes(x=(population),y=var,colour=controller,group=controller)) + geom_point()
# stat_summary(fun=mean, geom="line",size=0.8) 
head(k)


anova.k = aov(n~factor(population)+Error(factor(population)),data=subset(k,trial<3))

summary(anova.k)
k

g <- ggplot(k %>% 
group_by(population,controller) %>%
summarise(mean=mean(n/population)), aes(y=(mean),x=(population),colour=factor(controller))) + 
geom_line()+ geom_point() + 
      # geom_errorbar(aes(ymax=mean + error,ymin=mean-error,colour=factor(controller)),width=5,,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents per Cluster\nper Population Size",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g


library(ggplot2)
g <- ggplot(df1 %>% 
group_by(population,controller) %>%
summarise(mean=mean(n),sd=sd(n),error = qt((0.90 + 1)/2, df = n() - 1) * sd / sqrt(n())), aes(y=(mean),x=(population),colour=factor(controller))) + 
geom_line()+ geom_point() + 
      # geom_errorbar(aes(ymax=mean + error,ymin=mean-error,colour=factor(controller)),width=5,,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents per Cluster\nper Population Size",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggplot(aggregate(phat ~ population + controller,  k.means,mean),aes(x=population,y=phat,))

g <- ggplot(k %>% 
group_by(population,controller) %>%
summarise(mean=mean(n),sd=sd(n),error = qt((0.90 + 1)/2, df = n() - 1) * sd / sqrt(n())), aes(y=mean,x=population,colour=factor(controller))) + 
geom_line()+ geom_point() + 
      geom_errorbar(aes(ymax=mean + error,ymin=mean-error,colour=factor(controller)),width=5,,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents per Cluster\nper Population Size",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g





ggsave("agents_per_cluster_vs_population.png",g)


k$phat = predict(negative.binomial.model, k, type = "response")



newdata2 <- data.frame(
  math = rep(seq(from = min(dat$math), to = max(dat$math), length.out = 100), 3),
  prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
  levels(dat$prog)))

newdata2 <- cbind(newdata2, predict(m1, newdata2, type = "link", se.fit=TRUE))
newdata2 <- within(newdata2, {
  DaysAbsent <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})

k %>% 
group_by(population,controller) %>%
summarise(mean=mean((n)),sd=sd(n),est=mean((phat))) %>%
ggplot(aes(y=mean,x=est,colour=factor(controller))) + geom_point() 

k %>% 
group_by(population,controller,type,resources) %>%
summarise(mean=mean((n)),est=mean((phat),res=mean(mean-phat))) %>%
ggplot(aes(x=type,y=mean-est,colour=factor(population))) + geom_point() 



plot(fitted(negative.binomial.model),negative.binomial.model$residuals)



g <- ggplot(k, aes(y=n,x=population,colour=factor(controller))) + geom_point()+
# geom_smooth(method = "glm", se = FALSE,
#        method.args = list(family = "poisson"), linetype = "dashed")+
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents per Cluster\nper Population Size",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g
head(k$phat)
g + geom_line()



g<-ggplot(k %>% 
group_by(resources,environment, population,type,controller,trial) %>%
summarise(largest=max(n)) %>%
group_by(population,controller) %>%
summarise(mean=mean(largest), error = qt((0.90 + 1)/2, df = n() - 1) * sd(largest) / sqrt(n())), aes(y=mean,x=population,colour=factor(controller))) + 
geom_line()+ geom_point() + 
      geom_errorbar(aes(ymax=mean + error,ymin=mean-error),width=0.3,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents in Largest Cluster\nper Population Size",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("agents_in_largest_cluster_vs_population.png",g)

g<-ggplot(k %>% 
group_by(resources,environment, population,type,controller,trial) %>%
summarise(largest=max(n)) %>%
group_by(type,controller) %>%
summarise(mean=mean(largest), error = qt((0.90 + 1)/2, df = n() - 1) * sd(largest) / sqrt(n())), aes(y=mean,x=nchar(type),colour=factor(controller))) + 
geom_line()+ geom_point() + 
      geom_errorbar(aes(ymax=mean + error,ymin=mean-error),width=0.1,alpha=0.8) +
 labs(x="Resource Types",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents in Largest Cluster\nper Resource Types",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("agents_in_largest_cluster_vs_resource_types.png",g)


g <- ggplot(k %>% 
group_by(type,controller) %>%
summarise(mean=mean(n),sd=sd(n),error = qt((0.90 + 1)/2, df = n() - 1) * sd / sqrt(n())), aes(y=mean,x=nchar(type),colour=factor(controller))) + 
geom_line()+ geom_point() + 
      geom_errorbar(aes(ymax=mean + error,ymin=mean-error,colour=factor(controller)),width=0.2,,alpha=0.8) +
 labs(x="Resource Types",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents per Cluster\nper Resource Types",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("agents_per_cluster_vs_resource_types.png",g)



g<-ggplot(k %>% 
group_by(resources,environment, population,type,controller,trial) %>%
summarise(largest=max(n)) %>%
group_by(resources,controller) %>%
summarise(mean=mean(largest), error = qt((0.90 + 1)/2, df = n() - 1) * sd(largest) / sqrt(n())), aes(y=mean,x=resources,colour=factor(controller))) + 
geom_line()+ geom_point() + 
      geom_errorbar(aes(ymax=mean + error,ymin=mean-error),width=0.1,alpha=0.8) +
 labs(x="Resources",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents in Largest Cluster\nper Resource Amount",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("agents_in_largest_cluster_vs_resource_amount.png",g)


g <- ggplot(k %>% 
group_by(resources,controller) %>%
summarise(mean=mean(n),sd=sd(n),error = qt((0.90 + 1)/2, df = n() - 1) * sd / sqrt(n())), aes(y=mean,x=resources,colour=factor(controller))) + 
geom_line()+ geom_point() + 
      geom_errorbar(aes(ymax=mean + error,ymin=mean-error,colour=factor(controller)),width=0.2,,alpha=0.8) +
 labs(x="Resources",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents per Cluster\nper Resource Amount",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("agents_per_cluster_vs_resource_amount.png",g)



g <- ggplot(k %>% 
group_by(population,type) %>%
summarise(mean=mean(n),sd=sd(n),error = qt((0.90 + 1)/2, df = n() - 1) * sd / sqrt(n())), aes(y=mean,x=population,colour=factor(type))) + 
geom_line()+ geom_point() + 
      geom_errorbar(aes(ymax=mean + error,ymin=mean-error,colour=factor(type)),width=0.2,,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Resource Type",fill="Controller",title="Average Number of Agents per Cluster\nper population size",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g

ggsave("agents_per_cluster_vs_population_by_res_type.png",g)



g<-ggplot(agent_data %>% count(resources,environment, population,type,controller,trial,cluster), 
aes(y=n,x=factor(population),colour=factor(controller))) + 
geom_boxplot() 
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Number of Agents in Largest Cluster\nper Population Size",caption=paste(cluster_description,"90% CI used.")) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))
g


ggplot(cluster_data, aes(y=mean,x=population)) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) +
      stat_summary(fun=mean, geom="point",aes(colour=factor(controller)), size=2) +

      geom_errorbar(aes(group=controller,colour=factor(controller),ymin=mean(mean)- qt((0.90 + 1)/2, df = sum(count) - 1) * sqrt(sum((sd^2))) / sqrt(sum(count)),ymax=mean(mean)+ qt((0.90 + 1)/2, df = count - 1) * sqrt(sum((sd^2))) / sqrt(sum(count))))
#   stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller,colour=factor(controller)),width=0,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Amount of Agents in Each Cluster\nper Population Size",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))

ggplot(cluster_data, aes(y=mean,x=population)) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) +
      stat_summary(fun=mean, geom="point",aes(colour=factor(controller)), size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,aes(group=controller,colour=factor(controller)),width=0,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Amount of Agents in Each Cluster\nper Population Size",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))

ggplot(cluster_data, aes(y=mean,x=population, colour=type,group=type)) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
      stat_summary(fun=mean, geom="point", size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Amount of Agents in Each Cluster\nper Population Size",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))

ggplot(cluster_data, aes(y=mean,x=population, colour=factor(resources),group=factor(resources))) + 
stat_summary(fun=mean, geom="line",size=0.8) +
   stat_summary(fun=mean, geom="point",alpha=0.5,size=2) +
      stat_summary(fun=mean, geom="point", size=2) +
  stat_summary(geom = "errorbar", fun.data = mean_se,width=0,alpha=0.8) +
 labs(x="Population",y="Agents",colour="Controller",fill="Controller",title="Average Amount of Agents in Each Cluster\nper Population Size",caption=cluster_description) + 
 theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5))

ggplot(cluster_data, aes(y=sd,x=population)) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 


ggplot(cluster_data, aes(y=SS/count,x=population))+
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 

library(binom)
k %>% group_by(resources,population,controller,types,environment,trial) %>% summarise(lvd= 1-sum(prop),n=n()) %>%
#ungroup() %>% group_by(resources,population,controller,types) %>% summarise(mean= mean(lvd),size=sum(n) ) %>%
u#ngroup() %>% group_by(population,controller) %>% summarise(mean= mean(mean),sd = sqrt( (mean*(1-mean))/sum(size))) 




ggplot(aes(y=lvd,x=resources,colour=factor(controller))) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 


k %>% group_by(resources,population,controller,types,environment,trial) %>% summarise(lvd= 1-sum(prop)) %>%
ggplot(aes(y=lvd,x=types,colour=factor(controller))) + #geom_point(alpha=0.05) +
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 


ggplot(df,aes(y=fitness,x=population,colour=factor(resources))) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(resources)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(resources)),alpha=0.5,size=2) 


ggplot(t,aes(y=cluster/population,x=population,colour=factor(resources))) + geom_point()+
stat_summary(fun=mean, geom="line",aes(colour=factor(resources)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(resources)),alpha=0.5,size=2) 

ggplot(t,aes(y=cluster,x=population,colour=factor(controller))) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 

ggplot(t,aes(y=mean/population,x=population,colour=factor(controller))) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 

ggplot(t,aes(y=sd/sqrt(n),x=population,colour=factor(controller))) + geom_point()
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 


ggplot(t,aes(y=cluster,x=factor(type),colour=factor(controller))) + geom_boxplot()
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 


ggplot(t,aes(y=cluster,x=(resources),colour=factor(controller))) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(controller)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(controller)),alpha=0.5,size=2) 


ggplot(t,aes(x=factor(type),y=cluster)) + geom_boxplot()
head(t)

ggplot(t,aes(y=cluster,x=resources,colour=factor(type))) + 
stat_summary(fun=mean, geom="line",aes(colour=factor(type)),size=0.8) +
   stat_summary(fun=mean, geom="point",aes(colour=factor(type)),alpha=0.5,size=2) 
   # stat_summary(fun=mean, geom="line",aes(y=freq, group=2,colour="red"),size=0.8) +
   # stat_summary(fun=mean, geom="point",aes(y=freq,group=2,colour="red"),alpha=0.5,size=2) 

head(test %>% count(population,trial, resources,environment,hashcode,cluster))

head(test)

head(aggregate(data=test,))

tail(rand_sim_df)
require(ggplot2)
g<-ggplot(data=rand_sim_df, aes(x=agents, y=mean,colour=type)) +
      geom_ribbon(aes(x=agents,ymin=lower,ymax=upper,fill=type),alpha=0.2,colour=NA) +
      # geom_line()+
      geom_point()+
      labs(x="Population",y="Levenshtein Similarity",colour = "Resource Types",fill="Resource Types",
      title="Average Levenshtein Similarity between all talking agents\nterms for Resources per Population Size",
      caption="\nWhere Levenshtein Similarity = 0 indicates terms are entirely dissimilar and\nLevenshtein Similarity = 1 indicates terms are identical.") +
       theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))
g


head(sim_df)

ggsave("levenshtein.png",g)

ggplot(sim_df,aes(x=type,y=mean,colour=type)) + geom_boxplot()

resABCD = create.plots.all(cols=c("A","B","C","D"))

resABC


resA = create.plots.all(cols=c("A"),TRUE)

rm(resABC)

randABC= create.plots.all(cols=c("A","B","C"),FALSE)
save(randABC, file = "ABC_rand.RData")

{
randABCD = create.plots.all(cols=c("A","B","C","D"),FALSE)
save(randABCD, file = "ABCD_rand.RData")
}

{
randAB= create.plots.all(cols=c("A","B"),FALSE)
save(randAB, file = "AB_rand.RData")
}
{
randA= create.plots.all(cols=c("A"),FALSE)  
save(randA, file = "A_rand.RData")
}


save(resABC, file = "ABC.RData")

load("ABCD_rand.RData")

load("clust_df.RData")

tail(clust.df)

for (agent in c(0.5,1:10)){
   for (env in 1:10){
      for (trial in 1:11){

         resABCD[[agent*50]][[env]][[trial]]$df$D = 1- (resABCD[[agent*50]][[env]][[trial]]$df$A + resABCD[[agent*50]][[env]][[trial]]$df$B + resABCD[[agent*50]][[env]][[trial]]$df$C)


      }
   }
}

load("ABC.RData")
head(resABC)
resABC[[25]][[500]][[2]][[1]]$df
g

for (agent in c(0.5,1:10)*50){
      for (res in c(1:4)*500){
         for (env in 1:10){
            for (trial in 1:11){
               

               flag <- TRUE
               tryCatch({
               fn = paste("clusters/",agent,"_",res,"_",env,"_",trial,end=".png")
               g= resA[[agent]][[res]][[env]][[trial]]$plot
               ggsave(fn,g)

               }
               , error=function(e) {
                  flag<<-FALSE
                  print(fn)
                  },warning=function(w) flag<<-FALSE)            

               if (!flag){
                  next
               } 
            
         }
      }
   }
}


for (agent in c(0.5,1:10)){
   for (res in c(1:4)*500){
      for (env in 1:10){
         for (trial in 1:11){
            

            flag <- TRUE
            tryCatch({
               
            randABC[[agent*50]][[res]][[env]][[trial]]$df$population = agent*50
            randABC[[agent*50]][[res]][[env]][[trial]]$df$env = env
            randABC[[agent*50]][[res]][[env]][[trial]]$df$trial = trial
            randABC[[agent*50]][[res]][[env]][[trial]]$df$res = res

            randABC[[agent*50]][[res]][[env]][[trial]]$df$type = "ABC" 

            randABC[[agent*50]][[res]][[env]][[trial]]$df$D = NA
            # resA[[agent*50]][[res]][[env]][[trial]]$df$B = NA
            # resA[[agent*50]][[res]][[env]][[trial]]$df$C = NA
            # randABC[[agent*50]][[res]][[env]][[trial]]$df$D = NA
               
               
               
               }, error=function(e) flag<<-FALSE)
            if (!flag){
               # print(paste("fail on trial",trl,collapse=" "))
               next
            } 

            # clust.df = rbind(clust.df,resAB[[agent*50]][[env]][[trial]]$df)

         }
      }
   }
}


rand.clust.df = data.frame()

rand.clust.df = subset(rand.clust.df,type!="ABC")
rm(randABCD)

save(rand.clust.df, file = "rand_clust_df.RData")

save(clust.df, file = "clust_df.RData")

load("rand_clust_df.RData") 
clust.df = subset(clust.df,type != "ABC")



unique(clust.df$type)
head(resABC[[agent*50]][[res]][[env]][[trial]]$df)



for (agent in c(0.5,1:10)){
   for (res in c(1:4)*500){

      for (env in 1:10){
         for (trial in 1:11){

            # clust.df = rbind(clust.df,resABC[[agent*50]][[res]][[env]][[trial]]$df)
            flag <- TRUE
            tryCatch({rand.clust.df = rbind(rand.clust.df,randABC[[agent*50]][[res]][[env]][[trial]]$df)}, error=function(e) flag<<-FALSE)
            if (!flag){
               print(paste("fail on trial",trial,collapse=" "))
               next
            } 
            
         
         }
      }
   }
}


tail(rand.clust.df)

head((clust.df))

save(clust.df, file = "clust_df.RData")
load("clust_df.RData") 

agent.prop = clust.df[c("prop","population","type","env","trial")]
agent.prop$freq = agent.prop$prop*agent.prop$population

g <- ggplot(data=agent.prop, aes(x=factor(population), y=freq/population,colour=type),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(group=type), size=0.8) +
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",alpha=0.5) +
 labs(x="Population",y="Proportion",color="Resource Types",title="Average Proportion of Agents within Cluster per Population\nsize with 95% CI",caption="\nFigure 1: Clusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))
g





ggsave("agentPerCluster.png",g)
library(plyr)
library(ggplot2)
g <- ggplot(data=count(clust.df,c("population","type","env","trial","res")), aes(x=factor(population), y=freq,colour=type),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(group=type), size=0.8) +
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",alpha=0.5) +
 labs(x="Population",y="Cluster Number",color="Resource Types",title="Average Amount of Clusters per\nPopulation size with 95% CI",caption="\nFigure 2: Clusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))
g

g <- ggplot(data=count(clust.df,c("population","type","env","trial","res")), aes(x=factor(res), y=freq,colour=type),alpha=0.4) + 
 stat_summary(fun=mean, geom="line", aes(group=type), size=0.8) +
    stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",alpha=0.5) +
 labs(x="Resources",y="Cluster Number",color="Resource Types",title="Average Amount of Clusters per\nPopulation size with 95% CI",caption="\nFigure 2: Clusters were plotted using Levenshtein Similarity K-Means.\nK was determined by the Silhouette method.") + 
 theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))
g


ggsave("clusterPerAgent.png",g)


g <- g + ggtitle(paste("Cluster plot of",agent_no,"agents (5 repetitions) &",res,"Resources\nusing Levenshtein Distance.",collapse=" ")) + theme(plot.title = element_text(hjust = 0.5))
g 

ggsave("cluster_ABCD_50_2000.png",g)






ggplot(data=aggregate(data=words,log(fitness) ~ log(population),mean), aes(x=population,y=fitness)) + geom_line()

words$freq = words$freq/words$population
words$freq = words$freq*words$population

ggplot(aggregate(data=words, fitness ~ freq + type,mean), aes(x=log(freq),y=(fitness),colour=type)) +
 geom_point(alpha=0.6) +
#  geom_line()+
   geom_smooth(method="lm",se=FALSE, aes(color=type)) 

words$freq = words$freq/words$population
words$freq = words$freq*words$population

# Plots total fitness of all agents depending on word type.
wordsABCD = language.stats.all(c("A","B","C","D"))
wordsABCD$type = rep("ABCD",length(wordsABCD[,1]))

{
   cols = c("fitness","freq","population","type","env","res")

   wordsA = language.stats.all(c("A"))
   wordsAB = language.stats.all(c("A","B"))
   wordsABC = language.stats.all(c("A","B","C"))
   wordsABCD = language.stats.all(c("A","B","C","D"))

   wordsA$type =  rep("A",length(wordsA[,1]))
   wordsAB$type = rep("AB",length(wordsAB[,1]))
   wordsABC$type = rep("ABC",length(wordsABC[,1]))
   wordsABCD$type = rep("ABCD",length(wordsABCD[,1]))

   words = rbind(wordsA[cols],wordsAB[cols],wordsABC[cols],wordsABCD[cols])
}
words = rbind(wordsA[cols],words)

save(words, file = "words.RData")
load("words.RData") 

words$prop = words$freq/words$population

fitSummary <- summarySE(words, measurevar="fitness", groupvars=c("type","population"))
freqSummary <- summarySE(words, measurevar="freq", groupvars=c("type","population"))

fitResSummary <- summarySE(words, measurevar="fitness", groupvars=c("type","res"))
freqResSummary <- summarySE(words, measurevar="freq", groupvars=c("type","res"))

data=aggregate(data=words, freq ~ population + res + type,mean)
g<-ggplot(data, aes(x=type,y=freq, colour=type)) +
 geom_boxplot() +
 ylab("Frequency of Shared Dialects")+
 xlab("Resource Types")  
g
ggsave("resourceTypesVfreq.png",g)

ggplot(words,aes(x=population,y=prop,col=type))+ geom_point()


g1<-ggplot(freqSummary, aes(x=population, y=(mean), col=type)) +
      geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)), width=.1) +
      geom_line()+
      geom_point()+
      labs(x="Population",y="Frequency",colour = "Resource Types",fill="Resource Types",
      title="Average amount of shared dialects per Population size\nwith 95% CI (N=160 per Population & Resource type)",
      caption="\nFrequency indicates the average amount agents that share a distinct dialect.\nEach population was tested with 4 Resource configurations [500,1000,1500,2000]\nin 40 unique environments.") +
       theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))
g1
g2 <-ggplot(freqSummary, aes(x=population, y=mean/population, col=type)) +
      geom_errorbar(aes(ymin=(mean-ci)/population, ymax=(mean+ci)/population), width=.1) +
      geom_line()+
      geom_point()+
      labs(x="Population",y="Proportion",colour = "Resource Types",fill="Resource Types",
      title=" Proportion of agents that share a dialect per Population size\nwith 95% CI (N=160 per Population & Resource type)",
      caption="\nProportion indicates the average proportion of a population that shares a distinct dialect.\nEach population was tested with 4 Resource configurations [500,1000,1500,2000]\nin 40 unique environments.") +
       theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))
g2
ggsave("freq.png",g1)
ggsave("prop.png",g2)

g <- grid.arrange(g1, g2, ncol=2)
ggsave("freqVpopGrid.png",g)

g<-ggplot(freqResSummary, aes(x=res, y=(mean), col=type)) +
      geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)), width=.1) +
      geom_line()+
      geom_point()+
      xlab("Resources") +
      ylab("Frequency")
g


g<-ggplot(fitResSummary, aes(x=res, y=(mean), col=type)) +
      geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)), width=.1) +
      geom_line()+
      geom_point()+
      xlab("Resources") +
      ylab("Fitness")
g

ggsave("fitVres.png",g)

data=aggregate(data=words, fitness ~ population + res + type,mean)
g<- ggplot(data=data,aes(color=type, x=type,y=fitness)) + geom_boxplot()
g
ggsave("termsVfitness.png",g)

head(fitSummary)

g<-ggplot(fitSummary, aes(x=population, y=(sd), col=type)) +
   #  geom_point(alpha=0.7)+
   geom_line()



df_means <- aggregate(data=words, fitness ~ type,mean)
max_res = max(abs(rnorm(1000000, mean=4, sd=2)))
g<-ggplot(fitSummary, aes(x=population, y=(mean)/max_res, col=type)) +
   #  geom_point(alpha=0.7)+
   geom_errorbar(aes(ymin=(mean-ci)/max_res, ymax=(mean+ci)/max_res), width=.1,alpha=0.4) +
   geom_line()+
   geom_point()+
   geom_hline(data=df_means,aes(col=type,yintercept=fitness/max_res),linetype="dashed")+
   labs(x="Population",y="Fitness",colour = "Resource Types",fill="Resource Types",
      title=" Average Fitness of agents per Population size\nwith 95% CI",
      caption="\nEach population was tested with 4 Resource configurations [500,1000,1500,2000]\nin 10 unique environments repeated 10 times per environment.") +
       theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))

   #  geom_smooth(method="lm") +
   # xlab("Population Size") + ylab("Fitness")
g

g<-ggplot(fitSummary, aes(x=population, y=(mean), col=type)) +
   #  geom_point(alpha=0.7)+
   geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)), width=.1,alpha=0.4) +
   geom_line()+
   geom_point()+
   geom_hline(data=df_means,aes(col=type,yintercept=fitness),linetype="dashed")+
   labs(x="Population",y="Fitness",colour = "Resource Types",fill="Resource Types",
      title=" Average Fitness of agents per Population size\nwith 95% CI",
      caption="\nFitness was normalised by taking the initial Fitness of all agents (10) and dividing\nit by the average Fitness.\nEach population was tested with 4 Resource configurations [500,1000,1500,2000]\nin 40 unique environments.") +
       theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))
g

words$total = words$fitness*words$population
data=aggregate(data=words,total~population+type,mean)
head(data)
g<-ggplot(data, aes(x=population, y=(total), col=type)) +
   # geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)), width=.1,alpha=0.4) +
   geom_line()+
   geom_point()+
   # geom_hline(data=df_means,aes(col=type,yintercept=fitness),linetype="dashed")+
   labs(x="Population",y="Fitness",colour = "Resource Types",fill="Resource Types",
      title=" Average Fitness of agents per Population size\nwith 95% CI",
      caption="\nFitness was normalised by taking the initial Fitness of all agents (10) and dividing\nit by the average Fitness.\nEach population was tested with 4 Resource configurations [500,1000,1500,2000]\nin 40 unique environments.") +
       theme(plot.title = element_text(face="bold",size=15,hjust = 0.5),plot.caption =element_text(face="italic",size=10,hjust = 0.5))
g
ggsave("fitness.png",g)


summary(aov(data=words, fitness ~ type))
summary(aov(data=df, fitness ~ type))

data=aggregate(data=words, fitness ~ res + population ,mean)
g <- ggplot(data=data, aes(y=fitness,x=population, colour=factor(res))) + geom_line()+ geom_point()+
  labs(x = "Population", y = "Fitness", color = "Resources")
g
ggsave("fitVpop(res)-new.png",g)

data=aggregate(data=words, freq ~ res + population +type,mean)
g<- ggplot(data=data, aes(y=freq,x=population/res, colour=type)) + geom_point(alpha=0.6) +
  labs(x = "Resource per Agent", y = "Frequency", color = "Resources")+
     geom_smooth(method="lm",aes(col=type),se=FALSE)

g
ggsave("freqVratio.png",g)

data=cbind(aggregate(data=data.frame(fitness = words$fitness, rate=words$res/words$population),
 fitness ~ rate ,mean),freq=aggregate(data=data.frame(freq = words$freq, rate=words$res/words$population), freq ~ rate ,mean)$freq)

ggplot(data=data,aes(x=freq,y=fitness, colour=rate)) + geom_point() + geom_smooth(method="lm",se=FALSE)

data=aggregate(data=words, fitness ~ res + population + type ,mean)
g<-ggplot(data=data, aes(x=res/population, y=fitness,colour= (population)),pch=21,color="black") + geom_point(aes(size=res),alpha=0.6) +
   geom_smooth(aes(col=res),se=FALSE) +
     labs(x = "Resource per Agent", y = "Fitness", color = "Population",size="Resources")
g
ggsave("fitnessVratio-new.png",g)

ggplot(data=data, aes(y=fitness, x=res/population)) + geom_point() +
   geom_smooth(se=FALSE)

words$freq = words$freq*words$population

g<-ggplot(aggregate(data=total, fitness ~ resources + population + hashcode + type,mean), aes(y=fitness,x=hashcode,colour=type)) + geom_boxplot()

g<- ggplot(data=data, aes(x=log(freq),y=log(fitness)))+
   geom_point(alpha=0.6) +
   # geom_line()+
   # geom_smooth(aes(col=type),se=FALSE) +
   geom_smooth(method="lm",alpha=0.6,se=FALSE) +
   xlab("ln(Frequency)") +
   ylab("Fitness")
g

val = 0
g<- ggplot(aggregate(data=subset(words,freq>val), fitness ~ freq + res + population+type,mean), aes(x=log10(freq),y=fitness,colour=type))+
   # geom_line()+
   geom_point(alpha=0.6) +
   geom_smooth(method="lm", aes(col=type),alpha=0.6,se=FALSE) 
   # geom_smooth(aes(col=type),se=FALSE) 
g

g<-ggplot(words, aes(x=normalize((freq*population)), y=normalize(fitness),col=type)) +
    geom_point()+
    xlab("log(Proportion)") + ylab("Fitness")+
    stat_smooth(aes(col=type),method='lm', alpha=0.2, se=FALSE)
      # geom_smooth(method="lm", aes(col=type),alpha=0.6) 
   #  xlim(c(0,0.05))
g


ggsave("freqVfitnessLog.png",g)

cor.test(words.data$freq, words.data$words.mean,method="spearman")
