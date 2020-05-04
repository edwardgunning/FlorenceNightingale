#R version 4.0.0 (2020-04-24)
library(HistData) # CRAN v0.8-6
library(tidyverse) # CRAN v1.3.0
library(lubridate) # CRAN v1.7.8
library(reshape2) # CRAN v1.4.4
library(ggtext) # [github::wilkelab/ggtext] v0.1.0

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
    Date=='1854-04-01' ~ "APRIL \n 1854",
    TRUE ~ toupper(month.name[month(Date)])), #### added the labels
    Disease.rad = sqrt(Disease.rate*12/pi), ## 12*calculating sqrt(AREA)/pi to give radial line
    Wounds.rad = sqrt(Wounds.rate*12/pi), #same
    Other.rad = sqrt(Other.rate*12/pi), #same
    bar1 = Wounds.rad, # inner-most bar (Wounds)
    bar2 = ifelse(Other.rad>bar1, Other.rad-bar1, 0), # middle bar (Other)
    bar3 = ifelse(Disease.rad>(bar1+bar2), Disease.rad-bar1-bar2, 0), # Preventible disease
    labelpos = ifelse(bar1+bar2+bar3>15,bar1+bar2+bar3, 15)) # just to adjust the labels for April, May, June
  

degreesStart <- (6/12)*360 #start at June 
radiansStart <- degreesStart * (pi/180) # convert to radians :- )

Nighting_df$textangle = 90 - 360 * (c(1:12)-0.5) /12 # angle the label outside each segment


### put some wounds to show where wounds would be in oct (as described in text)
line_Nov_df <- data.frame(x=c(10.5,11.5), y=rep((Nighting_df %>% filter(Month=="Nov"))$Other.rad,2), bar=rep("bar2",2))
line_Oct_df <- data.frame(x=c(9.5,10.5), y=rep((Nighting_df %>% filter(Month=="Oct"))$Other.rad,2), bar=rep("bar2",2))
line_Sept_df <- data.frame(x=c(8.5,9.5), y=rep((Nighting_df %>% filter(Month=="Sep"))$Other.rad,2), bar=rep("bar2",2))


Nighting_Plot1 <- melt(Nighting_df,measure.vars=c("bar1", "bar2", "bar3"), variable.name = "bar") %>%
  ggplot(aes(x=month(Date), y=value))+
  geom_bar(aes(fill=factor(bar, levels=c("bar3", "bar2", "bar1")),color=factor(bar, levels=c("bar3", "bar2", "bar1"))),stat="identity",width = 1, alpha=0.7)+
  coord_polar(start=radiansStart, clip = "off")+
  geom_text(aes(label=Label, y=labelpos, angle=textangle), color="black", size=2.4, position = "identity", vjust=-1)+
  theme_void()+
  geom_line(aes(x=x,y=y), data=line_Nov_df, size=0.8, color="darkslategrey")+
  geom_line(aes(x=x,y=y), data=line_Sept_df, size=0.8, color="darkslategrey")+
  geom_line(aes(x=x,y=y), data=line_Oct_df, size=0.8, color="darkslategrey")+
  labs(subtitle = "1 . <br> APRIL 1854 <span style='font-size:10pt'>TO </span>MARCH 1855")+
  theme(legend.position = "none",
        plot.subtitle = element_markdown(color = "black", hjust = 0.6, lineheight = 1.5, family="serif",margin = margin(t=55,b =-70), size=14),
        plot.margin = margin(-50,-70,50,-70))+
  scale_fill_manual(values=c("lightblue","darkslategrey", "tomato"))+
  scale_color_manual(values=c("lightblue","darkslategrey", "tomato"))+
  annotate(geom = "text", x=9.55, y=32, label="CRIMEA", size=2.2, col="black", fontface="italic")+
  annotate(geom = "text", x=6.2, y=9.7, label="BULGARIA", size=2.2, col="black", angle=90, fontface="italic")

#### Right hand plot done :-)

##### now do left hand plot
### gonna do same thing with bars.
# not exactly correct but the best I can do for now gonna keep thinking of a way to do it
Nighting_df2 <- Nightingale  %>%
  filter(Date>'1855-03-01') %>%
  mutate(Label=case_when(
    Date=='1856-01-01' ~ "JANUARY \n 1856",
    Date=='1855-04-01' ~ "APRIL \n 1854",
    TRUE ~ toupper(month.name[month(Date)])),
    Disease.rad = sqrt(Disease.rate*12/pi),
    Wounds.rad = sqrt(Wounds.rate*12/pi),
    Other.rad = sqrt(Other.rate*12/pi),
    bar1 = Other.rad,
    bar2 = ifelse(Wounds.rad>bar1, Wounds.rad-bar1, 0),
    bar3 = ifelse(Disease.rad>(bar1+bar2), Disease.rad-bar1-bar2, 0),
    labelpos = ifelse(bar1+bar2+bar3>15,bar1+bar2+bar3, 15))

Nighting_df2$textangle = 90 - 360 * (c(1:12)-0.5) /12

Nighting_Plot2 <- melt(Nighting_df2,measure.vars=c("bar1", "bar2", "bar3"), variable.name = "bar") %>%
  ggplot(aes(x=month(Date), y=value))+
  geom_bar(aes(fill=factor(bar, levels=c("bar3", "bar2", "bar1")),color=factor(bar, levels=c("bar3", "bar2", "bar1"))),stat="identity",width = 1, alpha=0.7)+
  coord_polar(start=radiansStart, clip = "off")+
  geom_text(aes(label=Label, y=labelpos, angle=textangle), color="black", size=1.8, position = "identity", vjust=-1)+
  theme_void()+
  #geom_line(data=September_Line_df, aes(x=x,y=y), color="lightblue", size=1.5)+
  #geom_line(data=November_Line_df, aes(x=x,y=y), color="tomato", size=1.5)+
  labs(subtitle = "2 . <br> APRIL 1855 <span style='font-size:10pt'>TO </span>MARCH 1856")+
  theme(legend.position = "none",
        plot.subtitle = element_markdown(color = "black", hjust = 0.5, lineheight = 1.5, family="serif",margin = margin(t=0,b = -50), size=14),
        plot.margin = margin(b=-100, t=5))+
  scale_fill_manual(values=c("lightblue","tomato", "darkslategrey"))+
  scale_color_manual(values=c("lightblue","tomato", "darkslategrey"))


###### create a text bit

text_words <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       title = "The Areas of the blue, red, & black wedges are each measured from \n   the centre as the common vertex.
The blue wedges measured from the centre of the circle represent area \n   for area the deaths from Preventable or Mitigable Zymotic diseases, the \n   red wedges measured from the centre the deaths from wounds, & the \n   black wedges measured from the centre the deaths from all other causes.
The black line across the red triangle in Nov. 1854 marks the boundary \n   of the deaths from all other causes during the month.
In October 1854, & April 1855, the black area coincides with the red, \n   in January & February 1856, the blue coincides with the black.
The entire areas may be compared by following the blue, the red, & the \n   black lines enclosing them.") +
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
       title = "DIAGRAM <span style='font-size:12pt'> OF THE</span> CAUSES <span style='font-size:12pt'>OF</span> MORTALITY",
       subtitle="<span style='font-size:10pt'>IN THE</span> ARMY <span style='font-size:10pt'>IN THE</span> EAST.")+
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_markdown(size = 16, color = "black", family="Trattatello", hjust = 0.5),
        plot.subtitle = element_markdown(size = 14, color = "black", hjust = 0.5, face = "bold"),
        plot.margin = margin(0, 0, 0, 0),
        plot.title.position = "plot")


p<-cowplot::plot_grid(title_words, without_title, rel_heights=c(0.1, 1), ncol=1)
p + theme(plot.margin = margin(r=-70))
ggsave(filename = "causesofmortality.png", device = "png", width = 13, height = 7.2)

foot_note <- ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       title = "Data: {HistData} package CRAN.", 
       subtitle="Submission for the YSS Florence Nightingale #dataviz competition")+
  theme_void()+
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_markdown(size = 12, color = "black", family="Comic Sans MS", hjust = 1),
        plot.subtitle = element_markdown(size = 12, color = "black", hjust = 0.5, family = "Comic Sans MS"),
        plot.margin = margin(0, 0, 0, 0))






