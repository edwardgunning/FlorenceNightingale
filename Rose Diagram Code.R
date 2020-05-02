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
    labelpos = ifelse(bar1+bar2+bar3>7,bar1+bar2+bar3, 7)) # just to adjust the labels for April, May, June
  

degreesStart <- (6/12)*360 #start at June 
radiansStart <- degreesStart * (pi/180) # convert to radians :- )

Nighting_df$textangle = 90 - 360 * (c(1:12)-0.5) /12 # angle the label outside each segment


### put some wounds to show where wounds would be in oct (as described in text)
line_Nov_df <- data.frame(x=c(10.5,11.5), y=rep((Nighting_df %>% filter(Month=="Nov"))$Other.rad,2), bar=rep("bar2",2))
line_Oct_df <- data.frame(x=c(9.5,10.5), y=rep((Nighting_df %>% filter(Month=="Oct"))$Other.rad,2), bar=rep("bar2",2))
line_Sept_df <- data.frame(x=c(8.5,9.5), y=rep((Nighting_df %>% filter(Month=="Sep"))$Other.rad,2), bar=rep("bar2",2))


RightHandPlot <- melt(Nighting_df,measure.vars=c("bar1", "bar2", "bar3"), variable.name = "bar") %>%
  ggplot(aes(x=month(Date), y=value))+
  geom_bar(aes(fill=factor(bar, levels=c("bar3", "bar2", "bar1")),color=factor(bar, levels=c("bar3", "bar2", "bar1"))),stat="identity",width = 1, alpha=0.7)+
  coord_polar(start=radiansStart, clip = "off")+
  geom_text(aes(label=Label, y=labelpos, angle=textangle), color="black", size=1.9, position = "identity", vjust=-1)+
  theme_void()+
  geom_line(aes(x=x,y=y), data=line_Nov_df, size=0.6, color="darkslategrey")+
  geom_line(aes(x=x,y=y), data=line_Sept_df, size=0.6, color="darkslategrey")+
  geom_line(aes(x=x,y=y), data=line_Oct_df, size=0.6, color="darkslategrey")+
  labs(subtitle = "1 . <br> APRIL 1854 <span style='font-size:8pt'>TO </span>MARCH 1855")+
  theme(legend.position = "none",
        plot.subtitle = element_markdown(color = "black", hjust = 0.6, lineheight = 1.5, family="serif",margin = margin(t=80,b =-90)),
        plot.margin = margin(-10,-50,-12,-50))+
  scale_fill_manual(values=c("lightblue","darkslategrey", "tomato"))+
  scale_color_manual(values=c("lightblue","darkslategrey", "tomato"))+
  annotate(geom = "text", x=9.55, y=20, label="CRIMEA", size=1.6, col="black", fontface="italic")+
  annotate(geom = "text", x=6.2, y=5, label="BULGARIA", size=1.4, col="black", angle=90, fontface="italic")

#### Right hand plot done :-)

 

without_title <- cowplot::plot_grid(left_hand, Nighting_Plot1, rel_widths = c(0.4,0.6))
cowplot::plot_grid(title_words, without_title, rel_heights=c(0.1, 1), ncol=1)



