#R version 4.0.0 (2020-04-24)
library(readxl) # CRAN v1.3.1
library(tidyverse) # CRAN v1.3.0
library(reshape2) # CRAN v1.4.4
library(ggtext) # [github::wilkelab/ggtext] v0.1.0
library(extrafont) # CRAN v0.17 
library(scales) # CRAN v1.1.0 
### I've transcribed english mortality data from pg. 22 of the pdf
mort<- read_excel("EnglishMortalityData.xlsx")
colnames(mort) <- c("age", "n_soldier", "n_english", "d_soldier", "d_english")

#### we want to just seperate these into dying & living
#### can ignore deaths 
##### we're working at the start of each year
living <- mort %>% select(-c(d_soldier, d_english)) %>%
  melt(id.vars=c("age"), variable.name="mort_group", value.name = "living") %>%
  mutate(mort_group=recode_factor(mort_group, "n_soldier"="1. ENGLISH SOLDIERS.", "n_english"="2. ENGLISHMEN"))

dead <- mort %>% select(-c(d_soldier, d_english)) %>%
  mutate(dead_soldier=10000-n_soldier,
         dead_english=10000-n_english) %>%
  select(age, dead_soldier, dead_english) %>%
  melt(id.vars=c("age"), variable.name="mort_group", value.name = "dead") %>%
  mutate(mort_group=recode_factor(mort_group, "dead_soldier"="1. ENGLISH SOLDIERS.", "dead_english"="2. ENGLISHMEN"))

mort_df <- inner_join(dead, living, by = c("age", "mort_group"))


##### we can actually calculate this for age 40 too as we know how many died age 39

living_40 <- mort %>% filter(age==39) %>%
  select(-age) %>%
  mutate(age=c(40), n_soldier=n_soldier-d_soldier, n_english=n_english-d_english) %>% 
  select(-c(d_soldier, d_english)) %>%
  melt(id.vars=c("age"), variable.name="mort_group", value.name = "living") %>%
  mutate(mort_group=recode_factor(mort_group, "n_soldier"="1. ENGLISH SOLDIERS.", "n_english"="2. ENGLISHMEN"))

dead_40 <- mort %>% filter(age==39) %>%
  select(-age) %>%
  mutate(age=c(40), dead_soldier=10000-(n_soldier-d_soldier), dead_english=10000-(n_english-d_english)) %>%
  select(age, dead_soldier, dead_english) %>%
  melt(id.vars=c("age"), variable.name="mort_group", value.name = "dead") %>%
  mutate(mort_group=recode_factor(mort_group, "dead_soldier"="1. ENGLISH SOLDIERS.", "dead_english"="2. ENGLISHMEN"))

mort40_df <- inner_join(dead_40, living_40, by = c("age", "mort_group"))
mortplot_df <- bind_rows(mort_df, mort40_df)

####### data manipulation done - this was a nice one !
dead_text <- data.frame(label=rep("DEAD",2), x=rep(36.25,2), y=c(1250,750), mort_group=c("1. ENGLISH SOLDIERS.", "2. ENGLISHMEN"))

mortplot_df %>%
  melt(id.vars=c("age", "mort_group"), variable.name="status", value.name = "total") %>%
  mutate(status = factor(status, levels=c("living", "dead"))) %>%
  ggplot()+
  scale_x_continuous(position = "top", expand = c(0,0))+
  facet_wrap(~mort_group, ncol = 1, scales = "free")+
  scale_y_continuous(breaks = seq(0,10000, by=1000), labels=seq(10000, 0, by=-1000), expand = c(0,0))+
  geom_area(aes(x=age, y=total, fill=status), alpha=0.25)+
  theme_bw()+
  labs(title="(E). <br><span style='font-face:bold'><span style='font-family:Arial'><span style='font-size:9pt'> DIAGRAM</span></span> </span><br><span style='font-size:10pt'> *Showing the Numbers Living and Dead at the several Ages from 20-40*",
       caption = "Thus, the Diagram (1) shows that out of 10,000 soldiers at the age of 20, about \n7000 are living and 3000 are dead at the Age 40.",
       y="NUMBERS LIVING AND DEAD                                                    NUMBERS LIVING AND DEAD")+
  geom_line(aes(x=age, y=total), data=.%>%filter(status=="dead"))+
  theme(legend.position = "none", strip.placement = "outside",
        strip.background = element_blank(),
        plot.title = element_markdown(hjust = 0.5, family="serif"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face="italic", family="serif"),
        axis.text.y = element_text(face="italic", family="serif", size=12),
        plot.caption.position = "plot",
        plot.caption = element_text(family = "serif", face="italic", hjust=0, size=11, margin=margin(t=20)),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        #panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill=alpha("orange",0.15)),
        panel.grid.major = element_line(color='black'),
        axis.title.y = element_text(face="italic", size=7),
        strip.text = element_text(vjust = 3,family="Arial", face="bold", size=8),
        plot.margin = margin(r=10, l=10, t=2, b=2))+
  annotate(geom="text", x=28.75, y=6300, label="LIVING", fontface=2, size=2.5)+
  geom_text(aes(x=x,y=y,label=label), data=dead_text, size=2.5, fontface=2)+
  annotation_custom(textGrob("Age.", gp = gpar(col = "black", cex=0.8, fontface=3)), 
                    xmin=30, xmax=30,ymin=11050, ymax=11050)+
  coord_cartesian(clip = "off")

ggsave("AreaChart.png", device = "png", dpi=320, width = 7.49, height = 9.01)



