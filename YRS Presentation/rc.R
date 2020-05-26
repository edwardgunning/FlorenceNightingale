rc  <- Nightingale  %>%
  mutate(year_num = factor(ifelse(Date<='1855-03-01', "Y1", "Y2"), levels = c("Y1","Y2"),
                           labels=c(" 1 . <br> APRIL 1854 <span style='font-size:16pt'>TO </span>MARCH 1855",
                                    "2 . <br> APRIL 1855 <span style='font-size:16pt'>TO </span>MARCH 1856"))) %>%
  mutate(Label=case_when(
    Date=='1855-01-01' ~ "JANUARY 1855",
    Date=='1855-03-01' ~ "MARCH 1855",
    Date=='1854-04-01' ~ "APRIL 1854",
    Date=='1856-01-01' ~ "JANUARY \n 1856",
    Date=='1855-04-01' ~ "APRIL \n 1854",
    TRUE ~ toupper(month.name[month(Date)])), #### added the labels
    death_rate=(Disease.rate + Wounds.rate + Other.rate),
    label_pos = ifelse(sqrt(death_rate)>10, sqrt(death_rate), 10)) %>%
  ggplot()+
  aes(x=month(Date), y=sqrt(death_rate))+
  facet_wrap(~fct_rev(year_num))+
  theme_void()+
  theme(plot.margin = margin(b=30, r=-400,l=-400),
        panel.spacing = unit(-5, "lines"),
        strip.text = element_markdown(margin = margin(b=0, t=10), size = 20, family = "serif"),
        plot.title = element_markdown(size=24, hjust = 0.5, family = "serif", margin = margin(b=-20)),
        plot.subtitle = element_markdown(size=24, hjust = 0.5, margin = margin(b=-40, t=20)))+
  geom_bar(stat = "identity", width=1, fill="slategrey", alpha=0.3, color="slategrey")+
  coord_polar(theta = "x", clip="off", start=pi)+
  labs(title = "DIAGRAMS <span style='font-size:18pt'> OF THE</span> MORTALITY",
       subtitle="<span style='font-size:16pt'>IN THE</span> ARMY <span style='font-size:18pt'>IN THE</span> EAST.")+
  geom_hline(yintercept = sqrt(12), linetype="dotted")+
  geom_text(aes(label=Label), data=.%>% filter(as.integer(year_num)==1 & !(month(Date)%in%c(4:6))), angle=textangle[-c(1:3)], vjust=-1, size=4)+
  geom_text(aes(label=Label, y=2.5*sqrt(death_rate)), data=.%>% filter(as.integer(year_num)==1 & (month(Date)%in%c(4:6))), angle=alt_angle, size=4)+
  geom_text(aes(label=Label, y=label_pos), position = "identity", vjust=-1, data=.%>% filter(as.integer(year_num)==2), size=3, angle= textangle)+
  geom_text(aes(label=death_rate, y = sqrt(35)), angle=textangle2[-c(1:3)], data=.%>% filter(as.integer(year_num)==1 & !(month(Date)%in%c(4:6))), size=4.5)+
  geom_text(aes(label=death_rate, y = sqrt(85)), data=.%>% filter(as.integer(year_num)==2 &!(month(Date)%in%c(12,1,2,3,10,11))), size=4, angle=textangle2[1:6])+
  geom_text(aes(label=death_rate, y = sqrt(20)), data=.%>% filter(as.integer(year_num)==2 & month(Date)%in%c(10,11,12)), size=4, angle=textangle2[7:9])+
  geom_text(aes(label=death_rate, y = sqrt(5)), data=.%>% filter(as.integer(year_num)==2 & month(Date)%in%c(1,2,3)), size=2.8, angle=textangle2[10:12])+
  geom_text(data=anno_df, aes(x=x,y=y, label=label, angle=angle), size=3.5, fontface="italic")+
  geom_text(data=detailed_text, aes(x=x, y=y, label=d_text), size=6.5, hjust=0, family="Trattatello")