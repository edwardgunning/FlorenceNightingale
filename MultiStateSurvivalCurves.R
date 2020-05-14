library(tidyverse)
library(readxl)
library(reshape2)
tab_fa <- read_excel("Table Fa.xlsx")

tab_ea <- read_excel("Table EA.xlsx")

plot(tab_ea$Age,tab_ea$healthy_deaths)

tab_ea <- tab_ea %>%
  select(Age, healthy_living = `Englishmen Living`, healthy_deaths=`Englishmen Dying Yearly`)

tab_fa <- tab_fa %>% mutate(dead = lag(cumsum(Dying)),
                 inval = lag(cumsum(Invalided)),
                 death_inval_ratio=Invalided/Dying) %>%
  select(Age, living=Living, dead, inval, death_inval_ratio)

combined_tbl <- inner_join(tab_fa, tab_ea, by=c("Age")) %>%
  mutate(projected_inval = death_inval_ratio*healthy_deaths,
         healthy_dead = lag(cumsum(healthy_deaths)),
         healthy_inval = lag(cumsum(projected_inval)),
         healthy_living = 10000-(healthy_dead+healthy_inval)) %>%
  select(Age,living, dead, inval, healthy_living, healthy_dead, healthy_inval)

living_mlt <- combined_tbl %>%
  select(Age, army=living, englishmen=healthy_living) %>%
  melt(id.vars=c("Age"), variable.name="group", value.name = "n_living") %>%
  replace_na(list(n_living = list(10000)))

dead_mlt <- combined_tbl %>%
  select(Age, army=dead, englishmen=healthy_dead) %>%
  melt(id.vars=c("Age"), variable.name="group", value.name = "n_dead") %>%
  replace_na(list(n_dead = list(0)))

inval_mlt <- combined_tbl %>%
  select(Age, army=inval, englishmen=healthy_inval) %>%
  melt(id.vars=c("Age"), variable.name="group", value.name = "n_inval") %>%
  replace_na(list(n_inval = list(0)))

plt_df <- inner_join(living_mlt, dead_mlt, by=c("Age", "group")) %>% inner_join(inval_mlt, by=c("Age", "group"))

custom_y=c("0", paste(seq(1,10, by=1), "000", sep=","))
custom_x = c(20, 1:9,30, 1:9,40)

library(grid)
library(ggtext)
plt_df %>%
  mutate(n_dead=as.numeric(n_dead),
         n_living=as.numeric(n_living),
         n_inval=as.numeric(n_inval),
         ) %>%
  melt(id.vars=c("Age", "group"), variable.name="condition", value.name = "n_body") %>%
  mutate(condition=factor(condition, levels=c("n_dead", "n_inval", "n_living"))) %>%
  ggplot()+
  facet_wrap(~group, scales = "free")+
  aes(x=Age, y=n_body, fill=condition)+
  geom_line(data=. %>% filter(condition=="n_living"), color="darkgrey")+
  geom_line(aes(y=10000-n_body), data=. %>% filter(condition=="n_dead"), color="darkgrey")+
  geom_area(alpha=0.5)+
  geom_vline(xintercept = 30)+
  scale_x_continuous(expand = c(0,0), position = "top", breaks=seq(20,40, by=1),
                     labels = custom_x)+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,10000, by=1000),
                     labels = custom_y)+
  scale_fill_manual(values = c("cornflowerblue", "yellow", "tomato"))+
  coord_cartesian(clip="off", ylim = c(0,10000))+
  theme_light()+
  theme(legend.position = "none",
        panel.grid.major = element_line(color="darkgrey"),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(family="serif", face="italic"),
        axis.text.y = element_text(family="serif", face="italic", size=12),
        strip.text = element_blank(),
        panel.spacing = unit(10, "lines"),
        plot.margin = margin(b=20, t=20, r=20))+
  annotate(geom = "text", x = 25, y=-350, label="Young Soldiers", size=3.2, family="serif", fontface="italic")+
  annotate(geom = "text", x = 35, y=-350, label="Veterans", size=3.2, family="serif", fontface="italic")+
  annotate(geom = "text", x = 20.5, y=-350, label="20",family="serif", fontface="italic")+
  annotate(geom = "text", x = 30, y=-350, label="30", family="serif", fontface="italic")+
  annotate(geom = "text", x = 30, y=11200, label="Years", family="serif", fontface="italic")+
  annotate(geom = "text", x = 39, y=11200, label="Age", family="serif", fontface="italic")+
  annotate(geom = "text", x = 20.5, y=11200, label="Age", family="serif", fontface="italic")+
  annotate(geom = "text", x = 39.5, y=-350, label="40", family="serif", fontface="italic")



