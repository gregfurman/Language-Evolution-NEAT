library(dplyr)
library(ggplot2)

df %>% group_by(trial,resources,environment,split,world,cluster) %>% summarise(n=n()) %>%
ggplot(aes(y=n,x=factor(split))) + geom_boxplot()

sim_df = data.frame()

ggplot(df,aes())

for (f in list.files(".", pattern="controller_")){
   load(f)
   sim_df = dplyr::bind_rows(sim_df,df)
}

sim_df <- sim_df %>% rename(controller = hashcode) 

k=sim_df %>% group_by(resources,environment, population,type,controller,trial,cluster,world,split) %>%
summarise(n=n(),fitness=mean(fitness), var.fit=var(fitness))
k
head(sim_df)