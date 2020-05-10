library(tidyverse) # CRAN v1.3.0
library(HistData) # CRAN v0.8-6
library(lubridate) # CRAN v1.7.8
library(scales) # CRAN v1.1.0
library(hexSticker) # CRAN v0.4.6
library(showtext) # CRAN v0.7-1


font_add_google("Gochi Hand", "gochi")
showtext_auto()

hex <- Nightingale %>%
  filter(Date<='1855-03-01') %>%
  mutate(r=sqrt(Disease.rate+Wounds.rate+Other.rate)) %>%
  ggplot()+
  geom_bar(aes(x=month(Date), y=r), width=1, stat="identity", fill="maroon", color="navy", alpha=0.7)+
  coord_polar(theta="x", start = 0.5*2*pi)+
  theme_void()

sticker(hex, package="#FLOVIZ",
        p_size=10, s_x=1, s_y=0.9, s_width=2, s_height=1.75,
        filename="FloViz.png",
        white_around_sticker = F,
        h_color = "maroon", h_fill="white", p_color = "navy",
        p_family =  "gochi")



