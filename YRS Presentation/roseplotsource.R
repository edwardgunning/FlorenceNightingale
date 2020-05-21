textangle = 90 - 360 * (c(1:12)-0.5) /12
alt_angle <- 90+textangle[1:3]
textangle2 = 180+360/(2*pi)*rev( seq( pi/12, 2*pi-pi/12, len=12))
long_text <- "The dotted circle represents what the Mortality would have been had \n   the army been as healthy as Manchester - 12.4 per 1000 per annum. \nThe area of each Monthly division exhibits the relative mortality in the army \n   during the month. \nEach wedge admits of comparison, area for Area with every other wedge, and \n   with the Manchester Circle, and each wedge shows the Mortality per 1000 per \n   Annum for the Month. \n The dark Area outside the Manchester Circle exhibits the excess of Mortality in the \n   Army for the same ages over that of one of the most unhealthy Towns in England. \nThe figures show the Mortality per 1000 per annum."
detailed_text = data.frame(x=1.5, y=sqrt(1100), d_text=long_text, year_num="2 . <br> APRIL 1855 <span style='font-size:16pt'>TO </span>MARCH 1856")

anno_df <- data.frame(label=c("CRIMEA", "BULGARIA"), x=c(9.55, 6.385), y=c(19.2, 13.5), year_num=rep(" 1 . <br> APRIL 1854 <span style='font-size:16pt'>TO </span>MARCH 1855",2), angle=c(0, 90))
