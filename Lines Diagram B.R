#R version 4.0.0 (2020-04-24)
library(readxl) # CRAN v1.3.1
library(tidyverse) # CRAN v1.3.0
library(reshape2) # CRAN v1.4.4
library(ggtext) # [github::wilkelab/ggtext] v0.1.0
### I've transcribed english mortality data from pg. 22 of the pdf
mort<- read_excel("EnglishMortalityData.xlsx")
colnames(mort) <- c("age", "n_soldier", "n_english", "d_soldier", "d_english")
#### some manual aggregation/ manipulation
mort <- mort %>% mutate( 
  age_group=case_when(age>=20 & age<25 ~ "20-25",
                      age>=25 & age<30 ~ "25-30",
                      age>=30 & age<35 ~ "30-35",
                      age>=35 & age<40 ~ "35-40"),
  age_group = factor(age_group, levels = c("20-25","25-30", "30-35", "35-40")), # grouped by ages
  soldier_rate=(d_soldier/n_soldier)*1000,
  english_rate=(d_english/n_english)*1000) %>%
  select(age, age_group, soldier_rate, english_rate) %>%
  melt(id.vars=c("age", "age_group"), value.name = "rate", variable.name="mort_group") %>% #reshape it
  group_by(age_group, mort_group) %>%
  summarise(death_rate=mean(rate)) 

#### and plot
bounding_box <- data.frame(x=c(4.4,4.4), y=c(-12,0))
soldier_labels = mort %>% filter(mort_group=="soldier_rate")
soldier_labels$y=rep(-2.5, nrow(soldier_labels))
english_labels = mort %>% filter(mort_group=="english_rate")
english_labels$y=rep(-2.5, nrow(english_labels))
soldier_labels2 = mort %>% filter(mort_group=="soldier_rate")
soldier_labels2$label = rep("English Soldiers.", nrow(soldier_labels2))
soldier_labels2$y=rep(25, nrow(soldier_labels2))
english_labels = mort %>% filter(mort_group=="english_rate")
english_labels$y=rep(-2.5, nrow(english_labels))
english_labels2 = mort %>% filter(mort_group=="english_rate")
english_labels2$label = rep("Englishmen.", nrow(english_labels2))
english_labels2$y=rep(25, nrow(english_labels2))
mort %>% ggplot(aes(x=fct_rev(age_group)))+
  geom_bar(aes(fill=mort_group, y=death_rate),stat = "identity", position=position_dodge(0.5), width=0.15)+
  scale_fill_manual(values=c("red", "black"))+
  coord_flip()+
  geom_text(data=soldier_labels, aes(y=y, label=round(death_rate,1)), nudge_x = -0.15, family="serif")+
  geom_text(data=english_labels, aes(y=y, label=round(death_rate,1)), nudge_x = 0.15, family="serif")+
  geom_text(data=soldier_labels2, aes(y=y, label=label), nudge_x = -0.15, family="serif")+
  geom_text(data=english_labels2, aes(y=y, label=label), nudge_x = 0.15, family="serif")+
  geom_hline(yintercept = 0)+
  scale_y_continuous(limits = c(-12,30), expand = c(-0.025,1))+
  theme_light()+
  scale_x_discrete(expand = expand_scale(mult = c(0.2, 0.4)))+
  theme(legend.position = "none",
        axis.text.y = element_text(margin = margin(l = 40, r=-80), size=10, color="black", family="serif"),
        axis.title = element_blank(),
        axis.text.x=element_blank(),
        panel.grid = element_blank(),
        plot.subtitle = element_markdown(family="Times New Roman" , size=15, hjust = 0.5),
        plot.title = element_markdown(hjust = 0.5, family="serif"),
        plot.background = element_rect(fill=alpha("orange",0.15)),
        panel.background = element_blank(),
        plot.caption = element_markdown(family="serif", hjust=0.5, size=13),
        panel.border = element_rect(colour = "black", size=0.8))+
  annotate(geom="text",x = 4.55,y=5, label="DEATHS", family="serif", size=4)+
  annotate(geom="text",x = 4.8,y=-2.55, label="Deaths \n Annually to 1000 \nliving.", size=3, family="serif")+
  annotate(geom="text",x = 4.8,y=-9, label="AGES.", size=3, family="serif")+
  geom_line(data=bounding_box, aes(x=x,y=y))+
  geom_hline(yintercept = -5.3)+
  labs(title="(B.)<br><br><span style='font-size:52pt'><span style='font-family:RoyalGothic'>L</span></span><span style='font-size:30pt'><span style='font-family:LTCGoudyTextW01-Regular'>ines",
       subtitle = "*Representing the relative Mortality of the* **Army at Home** *and of the* **English Male Population** *at corresponding Ages.*",
       caption = "N<span style='font-size:10pt'>OTE</span>—The Mortality of the English Male Population at the above ages is taken from English Life Table (1949-1953)")


ggsave("LinesDiagramB.png", device = "png", dpi=320, width=12.8, height =7.68)


