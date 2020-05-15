library(tidyverse) # CRAN v1.3.0
library(readxl) # CRAN v1.3.1
library(reshape2) # CRAN v1.4.4
library(grid)
library(ggtext) # [github::wilkelab/ggtext] v0.1.0
library(cowplot) # CRAN v1.0.0
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

loss_df <- data.frame(label=rep("Loss",2), x=c(33,33), y=c(7700, 8400), condition=rep(NA,2), group=c("army","englishmen"))
by_df <- data.frame(label=rep("by",2), x=c(35.5,34.9), y=c(7000, 8150), condition=rep(NA,2), group=c("army","englishmen"))
in_text_df <- data.frame(label=rep("Invaliding",2), x=c(37.25,37.25), y=c(6150, 7700), condition=rep(NA,2), group=c("army","englishmen"))

multi_state <- plt_df %>%
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
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(family="serif", face="italic", size=11),
        axis.text.y = element_text(family="serif", face="italic", size=14),
        strip.text = element_blank(),
        panel.spacing = unit(8, "lines"),
        plot.background = element_rect(fill=alpha("burlywood1"), color="burlywood1"),
        plot.margin = margin(b=20, t=20, r=20))+
  annotate(geom = "text", x = 25, y=-350, label="Young Soldiers", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 35, y=-350, label="Veterans", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 20.5, y=-350, label="20",family="serif", fontface="italic")+
  annotate(geom = "text", x = 30, y=-350, label="30", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 30, y=11200, label="Years", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 39, y=11200, label="Age", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 20.5, y=11200, label="Age", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 39.5, y=-350, label="40", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 29.55, y=4500, label="Effectives", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 35.5, y=1500, label="Veterans", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 34, y=9300, label="Loss by", size=5, family="serif", fontface="italic")+
  annotate(geom = "text", x = 37, y=8700, label="Death", size=5, family="serif", fontface="italic")+
  geom_text(data=loss_df, aes(x=x,y=y, label=label), family="serif", fontface="italic", size=5)+
  geom_text(data=by_df, aes(x=x,y=y, label=label), family="serif", fontface="italic", size=5)+
  geom_text(data=in_text_df, aes(x=x,y=y, label=label), family="serif", fontface="italic", size=5)


lh_title <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       subtitle="*I. DIAGRAM - representing the ARMY <br> at Home in its present State.*")+
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_markdown(size = 14, color = "black", hjust = 0.5, family = "serif", margin = margin(r=50)),
        plot.margin = margin(0, 0, 0, 0))

rh_title <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       subtitle="*II.DIAGRAM - representing the ARMY <br> at Home in an improved State.*")+
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_markdown(size = 14, color = "black", hjust = 0.5, family = "serif", margin = margin()),
        plot.margin = margin(0, 0, 0, 0))

rh_title <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       subtitle="   *II.DIAGRAM - representing the ARMY <br>    at Home in an improved State.*")+
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_markdown(size = 14, color = "black", hjust = 0.7, family = "serif", margin = margin()),
        plot.margin = margin(0, 0, 0, 0))

(com_title=plot_grid(lh_title, rh_title, rel_widths = c(0.5,0.5), nrow=1)+
  theme(plot.margin = margin()))#,
       #plot.background = element_rect(color=alpha("orange", 0.2), fill=alpha("orange", 0.2))))


custom_rh_label= "This Diagram is constructed \n so as to show what the State \n of the Army at Home would \n be if the Mortality were the \n same as it is in the Civil \n Population at corresponding \n Ages, and if the Invaliding \n bore the same proportion to \n the Deaths as it does in \n Diagram I. \n The Invaliding may be more \n but it would probably be \n less than it is represented to \n be in Diagram II."

(final <- plot_grid(com_title, multi_state, ncol = 1, rel_heights = c(0.15, 0.85))+
  theme(plot.margin = margin(t=20, r=110, l=110))+
  annotate(x = 0.5, y=1.05, geom = "text", label="F.", family="serif")+
  annotate(x=0.5, y=0.5, geom = "text", label="Each of the \n 200 small \n Parallelograms \n represents \n 1000 Men.",
           family="serif", fontface="italic", size=6)+
  annotate(x=-0.115, y=0.6, geom = "text", label="I.", family="serif", size=6)+
  annotate(x=-0.05, y=0.6, geom = "text", label="This Diagram", family="serif", fontface="italic", size=6)+
  annotate(x=-0.059, y=0.53, geom = "text", label="has been constructed \n    to illustrate", family="serif", fontface="italic", size=4.5)+
  annotate(x=-0.055, y=0.45, geom = "text", label="Table F.a.", family="serif", fontface="italic", size=5)+
  annotate(x=.9775, y=0.5, geom="text", label=custom_rh_label, size=3.8, fontface="italic", family="serif", hjust=0)+
  annotate(x=0.985, y=0.765, geom="text", label="II.", size=4, family="serif", hjust=0))


caption <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       subtitle="<span style='font-family:sans'>TWO DIAGRAMS</span> showing the loss of strength in the <span style='font-family:sans'>ARMY</span> by <span style='font-family:sans'>INVALIDING</span>  and by <span style='font-family:sans'>DEATH.")+
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_blank(),
        plot.subtitle = element_markdown(size = 13, color = "black", hjust = 0.5, family = "serif", margin = margin(t=20), face = "italic"),
        plot.margin = margin(0, 0, 0, 0))

p<-plot_grid(final, caption, ncol = 1, rel_heights = c(0.9,0.1))

ggdraw()+
  draw_plot(p)+
  theme(panel.background = element_rect(fill="burlywood1", color="burlywood1"))

ggsave(filename = "mulistate_chart.png", device = "png", width = 15.5, height = 6.75)


