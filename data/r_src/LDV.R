library(dplyr)
require(tidyverse)
library(ggplot2)
library(stargazer)
library(MASS)
library(nlme)
library(betareg)
library(lmtest)

load("count_data.RData")

k$prop = (k$n/k$population)^2
k$types = nchar(k$type)

y.transf.betareg <- function(y){
    n.obs <- sum(!is.na(y))
    (y * (n.obs - 1) + 0.5) / n.obs
}

df = k %>%
   group_by(controller,population,types,resources) %>%
   filter(!(abs(n - median(n)) > 2*sd(n))) %>% ungroup() %>% 
   group_by(resources,population,controller,types,environment,trial) %>% summarise(lvd= 1-sum(prop),n=n()) %>%
   ungroup()

lvd_logit <- betareg(y.transf.betareg(lvd) ~ resources + log(population) + types + controller, data = df)
lvd_lm <- lm(lvd ~ resources + log(population) + types + controller, data =df)

summary(lvd_logit,type="response")
head(predict(lvd_logit, type = "quantile", at = c(0.05, 0.5, 0.95)))

lvd_logit_interaction <- betareg(y.transf.betareg(lvd) ~ resources + log(population) + types + controller +resources:log(population), link="logit" ,data = df)
lvd_lm_interaction <- lm(lvd ~ resources + log(population) + types + controller+resources*log(population), data =df)

summary(lvd_logit_interaction,type="response")
summary(lvd_lm_interaction)

stargazer(coeftest(lvd_logit),(lvd_lm),type="html",omit=c("phi"),
omit.stat=c("rsq","adj.rsq","f","n"),ci=TRUE, ci.level=0.90, single.row=TRUE,
add.lines = list(c("R2",paste("(pseudo)",round(lvd_logit$pseudo.r.squared,4)),round(summary(lvd_lm)$r.squared,4)),
c("Mean Absolute Error",round(mean(abs(df$lvd-predict(lvd_logit,df,type="response"))),4)
,round(mean(abs(df$lvd-predict(lvd_lm,df,type="response"))),4)),c("Observations",lvd_logit$n,lvd_logit$n)),
dep.var.labels=c("Beta Regression","OLS Regression"),
covariate.labels=c("Number of Resource","ln(Population Size)","Number of Resource Types","No Controller"))


stargazer(coeftest(lvd_logit_interaction),(lvd_lm_interaction),type="text",omit=c("phi"),
omit.stat=c("rsq","adj.rsq","f","n"),ci=TRUE, ci.level=0.90, single.row=TRUE,
add.lines = list(c("R2",paste("(pseudo)",round(lvd_logit_interaction$pseudo.r.squared,4)),round(summary(lvd_lm_interaction)$r.squared,4)),
c("Mean Absolute Error",round(mean(abs(df$lvd-predict(lvd_logit_interaction,df,type="response"))),4)
,round(mean(abs(df$lvd-predict(lvd_lm_interaction,df,type="response"))),4)),c("Observations",lvd_logit_interaction$n,lvd_logit_interaction$n)),
dep.var.labels=c("Beta Regression","OLS Regression"),
covariate.labels=c("Number of Resource","ln(Population Size)","Number of Resource Types","No Controller"))

round(mean(abs(df$lvd-predict(lvd_logit_interaction,df,type="response"))),4)

lvd_gls_log <- nlme::gls(lvd ~ resources + log(population) + types + controller, data = as.data.frame(df),
 na.action=na.omit,
               control = list(singular.ok = TRUE))
summary(lvd_gls_log)
# https://www.nature.com/articles/s41467-019-09842-2#Sec8
# file:///tmp/mozilla_gregory0/5510-12173-2-PB.pdf
# https://bmcecolevol.biomedcentral.com/articles/10.1186/s12862-018-1238-6


g<-df %>% group_by(population,controller) %>% summarise(mean=mean(lvd),ci=1.96*sqrt(mean*(1-mean)/sum(n))) %>%
ggplot(aes(y=mean,x=(population),colour=factor(controller))) + 
geom_point(alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci))) +
labs(x="Population",y="Linguistic Divsersity Index",colour="Controller",fill="Controller",title=" Average Linguistic Diversity Index of Agents within Clusters per Population Size",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g


df.predictions = unique(df[,c("population","resources")])
df.predictions$controller = "ANN"
df.predictions$types = 0
df.predictions

df.predictions$beta = predict(lvd_logit_interaction,df.predictions,type=c("response"))

df.predictions %>% group_by(population) %>% summarise(diff=diff(beta))

filter(df.predictions,resources==1000)$beta - filter(df.predictions,resources==1500)$beta 


df.predictions = unique(df[,c("population","types","resources","controller")])
df.predictions$beta = predict(lvd_logit_interaction,df.predictions,type=c("response"))
df.predictions$ols = predict(lvd_lm_interaction,df.predictions,type=c("response"))
# df.predictions = cbind(df.predictions,predict(lvd_logit,df.predictions ,type = "quantile", at = c(0.05, 0.5, 0.95)))

df.predictions

df %>% group_by(resources,population) %>% summarise(ratio = resources/population, mean=mean(lvd)) %>%
ggplot(aes(x=ratio,y=mean,color=factor(population),shape=factor(resources))) + geom_point() + geom_smooth(method="lm")+
labs(x="Ratio of Resources to Populatio",y=" Predicted Linguistic Divsersity Index",colour="Population Size",shape="Resource Amount",title=" Predicted Average Linguistic Diversity Index\nof Agents per Resource Amount",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 




subset(df.predictions,population==50)
g<- df.predictions %>% group_by(resources,population) %>% summarise(lvd=mean(beta)) %>%
ggplot(aes(y=lvd,x=resources,color=factor(population))) + geom_point() + geom_line() +
labs(x="Resource Amount",y=" Predicted Linguistic Divsersity Index",colour="Population Size",title=" Predicted Average Linguistic Diversity Index\nof Agents per Resource Amount",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("pred_lvd_vs_res_by_pop.png",g)

g<- subset(df.predictions,population>200) %>% group_by(resources,population) %>% summarise(lvd=mean(beta)) %>%
ggplot(aes(y=lvd,x=resources,color=factor(population))) + geom_point() + geom_line() +
labs(x="Resource Amount",y=" Predicted Linguistic Divsersity Index",colour="Population Size",title=" Predicted Average Linguistic Diversity Index\nof Agents per Resource Amount",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("pred_lvd_vs_res_by_pop>200.png",g)

g<-df %>% group_by(population,controller) %>% summarise(mean=mean(lvd),ci=1.96*sqrt(mean*(1-mean)/sum(n))) %>%
ggplot(aes(y=mean,x=log(population) ,colour=factor(controller))) + 
geom_point(alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci))) +
labs(x="Population",y="Linguistic Divsersity Index",colour="Controller",fill="Controller",title=" Average Linguistic Diversity Index of Agents within Clusters\nper log(Population Size)",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("lvd_v_log_pop.png",g)

g<-df %>% group_by(resources,controller) %>% summarise(mean=mean((lvd) ),ci=1.96*sqrt(mean*(1-mean)/sum(n))) %>%
ggplot(aes(y=mean,x=resources,colour=factor(controller))) + 
geom_point(alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)),width=100) +
labs(x="Resources",y="Linguistic Divsersity Index",colour="Controller",fill="Controller",title=" Average Linguistic Diversity Index of Agents within Clusters\nper Resource Amount",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("lvd_v_resources.png",g)


g<-df %>% group_by(types,controller) %>% summarise(mean=mean(lvd),ci=1.96*sqrt(mean*(1-mean)/sum(n))) %>%
ggplot(aes(y=mean,x=types,colour=factor(controller))) + 
geom_point(alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)),width=0.5) +
labs(x="Resource Types",y="Linguistic Divsersity Index",colour="Controller",fill="Controller",title=" Average Linguistic Diversity Index of Agents within Clusters\nper Number of Resource Types present",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

ggsave("lvd_v_res_types.png",g)


g<-df %>% group_by(population,types) %>% summarise(mean=mean((lvd)),ci=1.96*sqrt(mean*(1-mean)/sum(n))) %>%
ggplot(aes(y=mean,x=types,colour=factor(population))) + 
geom_point(alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)),width=0.5) +
labs(x="Resource Types",y="Linguistic Divsersity Index",colour="Controller",fill="Controller",title=" Average Linguistic Diversity Index of Agents within Clusters\nper Number of Resource Types present",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g



g<-subset(df,population>100) %>% group_by(population,resources) %>% summarise(mean=mean(lvd),ci=1.96*sqrt(mean*(1-mean)/sum(n))) %>%
ggplot(aes(y=mean,x=(resources) ,colour=factor(population))) + 
geom_point(alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)),width=20) +
labs(x="Population",y="Linguistic Divsersity Index",colour="Controller",fill="Controller",title=" Average Linguistic Diversity Index of Agents within Clusters\nper log(Population Size)",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g


g<-subset(df,population>=150) %>% group_by(controller,resources) %>% summarise(mean=mean(lvd),ci=1.96*sqrt(mean*(1-mean)/sum(n))) %>%
ggplot(aes(y=mean,x=(resources) ,colour=factor(controller))) + 
geom_point(alpha=0.5) + geom_line() + geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)),width=20) +
labs(x="Population",y="Linguistic Divsersity Index",colour="Controller",fill="Controller",title=" Average Linguistic Diversity Index of Agents within Clusters\nper log(Population Size)",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g



df %>% group_by(population) %>%
  t_test(lvd ~ controller, paired = TRUE) %>%
  add_significance()

df %>% group_by(resources) %>%
  t_test(lvd ~ controller, paired = TRUE) %>%
  add_significance()

df %>% group_by(types) %>%
  t_test(lvd ~ controller, paired = TRUE) %>%
  add_significance()

df %>% group_by() %>%
  t_test(lvd ~ controller, paired = TRUE) %>%
  add_significance()

df %>% group_by() %>%
  f_test(lvd ~ controller + population, paired = TRUE) %>%
  add_significance()

wilcox.test(lvd ~ controller, df)

boxplot(filter(df,controller=="ANN")$lvd-filter(df,controller=="Random")$lvd)

k

t.test(prop ~ controller, k,paired=TRUE)

library(plotly)


fig <- plot_ly() %>% add_trace(data=df.predictions,
 y = ~log(population), x = ~resources, z = ~beta, color = ~beta, intensity=~beta, type="mesh3d",colors = colorRamp(c("blue", "lightblue", "chartreuse3", "yellow", "red"))) %>% 
 layout(scene = list(xaxis = list(title = 'ln(Population Size'),
                     yaxis = list(title = 'Resource Number'),
                     zaxis = list(title = 'Predicted Linguistic Diversity Index')))

fig


g<- df %>% 
ggplot(aes(y=lvd,x=(log(population)/resources))) + 
geom_point(alpha=0.5) #+ geom_line() + geom_errorbar(aes(ymin=(mean-ci), ymax=(mean+ci)),width=20) +
labs(x="Population",y="Linguistic Divsersity Index",colour="Controller",fill="Controller",title=" Average Linguistic Diversity Index of Agents within Clusters\nper log(Population Size)",
caption="Clusters were formed using Complete Linkage Hierarchical Clustering on Levenshtein Dissimilarity Matrices.\n\nWhere an LDI of 1 indicates the most diversity with 0 indicaing the least.") +
theme(plot.title = element_text(face="bold",size=title_size,hjust = 0.5),plot.caption =element_text(face="italic",size=lab_size,hjust = 0.5)) 
g

df.predictions$ratio = df.predictions$resources/df.predictions$population

df.predictions

plot(df.predictions$ratio,y=df.predictions$beta)

fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                     yaxis = list(title = 'Gross horsepower'),
                     zaxis = list(title = '1/4 mile time')))

htmlwidgets::saveWidget(fig, "3dplot.html", selfcontained=FALSE)

fig