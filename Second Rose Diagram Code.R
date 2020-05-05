#R version 4.0.0 (2020-04-24)
library(HistData) # CRAN v0.8-6
library(tidyverse) # CRAN v1.3.0
library(lubridate) # CRAN v1.7.8
library(reshape2) # CRAN v1.4.4
library(ggtext) # [github::wilkelab/ggtext] v0.1.0
library(cowplot) # CRAN v1.0.0
library(extrafont) # CRAN v0.17
#### data is contained in HistData package luckily.
### get it set up.
#### idea is to make a stacked histogram and use coord_polar to convert to polar area chart
# going to start with the chart on the rights o filter this data
# areas are stacked over eachother as opposed to on top of each other so 
## (we'll work around this creating our own bars - 'bar1, bar2, bar3 etc.)

Nighting_df <- Nightingale  %>%
  filter(Date<='1855-03-01') %>% # first diagram
  mutate(Label=case_when(
    Date=='1855-01-01' ~ "JANUARY 1855",
    Date=='1855-03-01' ~ "MARCH 1855",
    Date=='1854-04-01' ~ "APRIL 1854",
    TRUE ~ toupper(month.name[month(Date)])), #### added the labels
    deathrate=(Disease.rate + Wounds.rate + Other.rate),
    death_radline = sqrt(Disease.rate + Wounds.rate + Other.rate),
    labelpos = ifelse(death_radline>sqrt(40),death_radline, sqrt(40))) # just to adjust the labels for April, May, June

Manchesterrate = 12.4 # per annum
degreesStart <- (6/12)*360 #start at June 
radiansStart <- degreesStart * (pi/180) # convert to radians :- )
Nighting_df$textangle = 90 - 360 * (c(1:12)-0.5) /12 # angle the label outside each segment
Nighting_df$textangle[1:3] <- 90+Nighting_df$textangle[1:3]
Nighting_df$textangle2 = 180+360/(2*pi)*rev( seq( pi/12, 2*pi-pi/12, len=12))
Manchester_Data <- data.frame(y=rep(sqrt(Manchesterrate),100), x=seq(0.5,12.5, length.out = 100))
Nighting_Plot1 <- Nighting_df %>%
  ggplot(aes(x=month(Date), y=death_radline))+
  geom_bar(stat="identity",width = 1, alpha=0.4, fill="slategrey", color="slategrey")+
  coord_polar(start=radiansStart, clip = "off")+
  geom_text(aes(label=Label, y=labelpos, angle=textangle), color="black", size=2.4, position = "identity", vjust=-1,data=.%>% filter(!(month(Date)%in%c(4,5,6))))+
  geom_text(aes(label=Label, y=3*death_radline, angle=textangle, x=month(Date)+0.25), color="black", size=2.4, position = "identity", vjust=-1,data=.%>% filter(month(Date)%in%c(4,5,6)))+
  theme_void()+
  geom_line(aes(x=x,y=y), data=Manchester_Data, linetype="dotted")+
  geom_text(aes(label=deathrate, angle=textangle2,y = sqrt(35)), data=.%>% filter(!(month(Date)%in%c(4,5,6))), size=2.5)+
  labs(subtitle = "1 . <br> APRIL 1854 <span style='font-size:10pt'>TO </span>MARCH 1855")+
  theme(legend.position = "none",
        plot.subtitle = element_markdown(color = "black", hjust = 0.6, lineheight = 1.5, family="serif",margin = margin(t=55,b =-70), size=14),
        plot.margin = margin(-50,-70,50,-70))+
  scale_fill_manual(values=c("lightblue","darkslategrey", "tomato"))+
  scale_color_manual(values=c("lightblue","darkslategrey", "tomato"))+
  annotate(geom = "text", x=9.55, y=20, label="CRIMEA", size=2.2, col="black", fontface="italic")+
  annotate(geom = "text", x=6.35, y=12.2, label="BULGARIA", size=2.2, col="black", angle=90, fontface="italic")




##### now do left hand plot
### gonna do same thing with bars.
# not exactly correct but the best I can do for now gonna keep thinking of a way to do it
Nighting_df2 <- Nightingale  %>%
  filter(Date>'1855-03-01') %>%
  mutate(Label=case_when(
    Date=='1856-01-01' ~ "JANUARY \n 1856",
    Date=='1855-04-01' ~ "APRIL \n 1854",
    TRUE ~ toupper(month.name[month(Date)])),
    deathrate=(Disease.rate + Wounds.rate + Other.rate),
    death_radline = sqrt(Disease.rate + Wounds.rate + Other.rate),
    labelpos = ifelse(death_radline>sqrt(100),death_radline, sqrt(100)))

Nighting_df2$textangle = 90 - 360 * (c(1:12)-0.5) /12 # angle the label outside each segment
Nighting_df2$textangle2 = 360/(2*pi)*rev( seq( pi/12, 2*pi-pi/12, len=12))

Nighting_Plot2 <- Nighting_df2 %>%
  ggplot(aes(x=month(Date), y=death_radline))+
  geom_bar(stat="identity",width = 1, alpha=0.4, fill="slategrey", color="slategrey")+
  coord_polar(start=radiansStart, clip = "off")+
  geom_text(aes(label=Label, y=labelpos, angle=textangle), color="black", size=2.2, position = "identity", vjust=-1)+
  theme_void()+
  geom_line(aes(x=x,y=y), data=Manchester_Data, linetype="dotted")+
  geom_text(aes(label=deathrate, angle=textangle2,y = sqrt(90)), data=.%>% filter(!(month(Date)%in%c(12,1,2,3,10,11))), size=2.5)+
  geom_text(aes(label=deathrate, angle=textangle2,y = sqrt(20)), data=.%>% filter((month(Date)%in%c(10,11,12))), size=2.5)+
  geom_text(aes(label=deathrate, angle=textangle2,y = sqrt(5)), data=.%>% filter((month(Date)%in%c(1,2,3))), size=2)+
  labs(subtitle = "2 . <br> APRIL 1855 <span style='font-size:10pt'>TO </span>MARCH 1856")+
  theme(legend.position = "none",
        plot.subtitle = element_markdown(color = "black", hjust = 0.5, lineheight = 1.5, family="serif",margin = margin(t=0,b = -50), size=14),
        plot.margin = margin(b=-100, t=5))


###### create a text bit

text_words <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       title = "The dotted circle represents what the Mortality would have been had \n   the army been as healthy as Manchester - 12.4 per 1000 per annum. \nThe area of each Monthly division exhibits the relative mortality in the army \n   during the month. \nEach wedge admits of comparison, area for Area with every other wedge, and \n   with the Manchester Circle, and each wedge shows the Mortality per 1000 per \n   Annum for the Month. \n The dark Area outside the Manchester Circle exhibits the excess of Mortality in the \n   Army for the same ages over that of one of the most unhealthy Towns in England. \nThe figures show the Mortality per 1000 per annum.
       ") +
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(size = 14, color = "black", family="Trattatello", face="italic"),
        plot.margin = margin(60, 400, 0, 100),
        plot.title.position = "plot")


left_hand <- cowplot::plot_grid(Nighting_Plot2, text_words, ncol=1, rel_heights = c(0.4,0.6))
without_title <- cowplot::plot_grid(left_hand, Nighting_Plot1, rel_widths = c(0.4,0.6))

without_title2 = without_title + theme(plot.margin=margin(b=-85))

title_words <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       title = "DIAGRAMS <span style='font-size:12pt'> OF THE</span> CAUSES <span style='font-size:12pt'>OF</span> MORTALITY",
       subtitle="<span style='font-size:10pt'>IN THE</span> ARMY <span style='font-size:10pt'>IN THE</span> EAST.")+
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_markdown(size = 16, color = "black", family="Trattatello", hjust = 0.5),
        plot.subtitle = element_markdown(size = 14, color = "black", hjust = 0.5, face = "bold"),
        plot.margin = margin(0, 0, 0, 0),
        plot.title.position = "plot")


(p <-cowplot::plot_grid(title_words, without_title, rel_heights=c(0.1, 1), ncol=1))

ggsave(filename = "rosediagram2.png", device = "png", width = 13, height = 7.2)


#### YSS Submission added details
other_title <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       title = "<span style='color:maroon'>YSS</span> #FLOVIZ  challenge")+
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_markdown(size = 40, color = alpha("navy",0.8), family="Trattatello"),
        plot.margin = margin(0, 0, 0, 0),
        plot.title.position = "panel")

q <-cowplot::plot_grid(other_title,p, rel_heights = c(0.1,0.9), ncol = 1)+ 
  theme(plot.margin = margin(r=-70, b=-40),
        plot.background = element_rect(fill=alpha("cornsilk",0.8)))



ggdraw()+
  draw_plot(q)+
  draw_plot_label("Data:    \n Twitter:",x = 0.87, hjust = 1, family="Trattatello", color="navy")+
  draw_plot_label("{HistData}, CRAN, \ngunning_edward",x = 0.99, hjust = 1, family="Trattatello", color="maroon")+
  #draw_image("Nightingale_Badge.jpg", x = 1, hjust = 1, width = 0.13, height = 1.8)+
  draw_line(x=c(0.125,0.35,0.63), y=c(0.52,0.35,0.44), color="black", size=0.8, linetype="dotted")
ggsave(filename = "YSSrosediagram2.png", device = "png", width = 13, height = 7.2)


