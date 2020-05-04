#R version 4.0.0 (2020-04-24)
library(readxl) # CRAN v1.3.1
library(tidyverse) # CRAN v1.3.0
library(reshape2) # CRAN v1.4.4
library(ggtext) # [github::wilkelab/ggtext] v0.1.0
### I've transcribed english mortality data from pg. 22 of the pdf
mort<- read_excel("EnglishMortalityData.xlsx")
colnames(mort) <- c("age", "n_soldier", "n_english", "d_soldier", "d_english")

#### we want to just seperate these into dying & living
#### can ignore deaths 
##### we're working at the start of each year
living <- mort %>% select(-c(d_soldier, d_english)) %>%
  melt(id.vars=c("age"), variable.name="mort_group", value.name = "living") %>%
  mutate(mort_group=recode_factor(mort_group, "n_soldier"="English Soldiers", "n_english"="Englishmen"))

dead <- mort %>% select(-c(d_soldier, d_english)) %>%
  mutate(dead_soldier=10000-n_soldier,
         dead_english=10000-n_english) %>%
  select(age, dead_soldier, dead_english) %>%
  melt(id.vars=c("age"), variable.name="mort_group", value.name = "dead") %>%
  mutate(mort_group=recode_factor(mort_group, "dead_soldier"="English Soldiers", "dead_english"="Englishmen"))

mort_df <- inner_join(dead, living, by = c("age", "mort_group"))


##### we can actually calculate this for age 40 too as we know how many died age 39

living_40 <- mort %>% filter(age==39) %>%
  select(-age) %>%
  mutate(age=c(40), n_soldier=n_soldier-d_soldier, n_english=n_english-d_english) %>% 
  select(-c(d_soldier, d_english)) %>%
  melt(id.vars=c("age"), variable.name="mort_group", value.name = "living") %>%
  mutate(mort_group=recode_factor(mort_group, "n_soldier"="English Soldiers", "n_english"="Englishmen"))

dead_40 <- mort %>% filter(age==39) %>%
  select(-age) %>%
  mutate(age=c(40), dead_soldier=10000-(n_soldier-d_soldier), dead_english=10000-(n_english-d_english)) %>%
  select(age, dead_soldier, dead_english) %>%
  melt(id.vars=c("age"), variable.name="mort_group", value.name = "dead") %>%
  mutate(mort_group=recode_factor(mort_group, "dead_soldier"="English Soldiers", "dead_english"="Englishmen"))

mort40_df <- inner_join(dead_40, living_40, by = c("age", "mort_group"))
mortplot_df <- bind_rows(mort_df, mort40_df)

####### data manipulation done - this was a nice one !

mortplot_df %>%
  melt(id.vars=c("age", "mort_group"), variable.name="status", value.name = "total") %>%
  mutate(status = factor(status, levels=c("living", "dead"))) %>%
  ggplot()+
  scale_x_continuous(position = "top", expand = c(0,0))+
  facet_wrap(~mort_group, ncol = 1, scales = "free")+
  scale_y_continuous(breaks = seq(0,10000, by=1000), labels=seq(10000, 0, by=-1000), expand = c(0,0))+
  geom_area(aes(x=age, y=total, fill=status), alpha=0.4)+
  theme_bw()+
  labs(title="(E). <br> DIAGRAM <br> *Showing the Numbers Living and Dead at the several Ages from 20-40*")+
  geom_line(aes(x=age, y=total), data=.%>%filter(status=="dead"))+
  theme(legend.position = "none", strip.placement = "outside",
        strip.background = element_blank(),
        plot.title = element_markdown(hjust = 0.5),
        axis.title.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color='black'),
        strip.text = element_text(vjust = 3.2))

