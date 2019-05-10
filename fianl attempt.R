title: "Final dta - Data Science"
author: "Andrés Ponce, Ben West, William Lott"
output: 
  html_document:
  toc: true # table of content true
toc_depth: 3  # upto three depths of headings (specified by #, ## and ###)
number_sections: true  ## if you want number sections at each table header
theme: united  # many options for theme, this one is my favorite.
---
  #Packages
library("WDI")
install.packages("WDI")
library("dplyr")
library(plyr)
library(ggplot2)
install.packages("reshape")
library(reshape)
#Preparting the Data
dat = WDI(indicator=c('NY.GDP.PCAP.KD', "SP.POP.TOTL", "MS.MIL.XPND.GD.ZS"), country='all', start=1960, end=2017)
dat <- dat[complete.cases(dat),]
dat2 <- dat
dat2 <- dat2[!duplicated(dat2[,c("country")]),]
country=c('MX','CA','US')

ccodes <- c("B8","T6","V2","1A", "Z4", "4E", "T4", "XC", "Z7", "7E", "T7", "EU", "F1", "XE", "XD", "XF", "ZT", "XH", "XI", "XG", "LA", "V3", "ZJ", "XJ", "T2", "XL", "XO", "XM", "XN", "ZQ", "XQ", "T3", "XP", "XU", "OE", "S4", "S2", "V4", "V1", "S1", "8S", "T5", "ZG", "ZF", "XT", "1W")

#create new dataframe
milexp <- dat %>% filter(!iso2c %in% ccodes)

#rename variables
milexp <- rename(milexp, c(NY.GDP.PCAP.KD="GDP", SP.POP.TOTL="Population",MS.MIL.XPND.GD.ZS="Military Exp"))

#Create a variable that is raw military expenditures and one that is the mean expenditure by percentage and by raw
#expenditures
milexp <- milexp %>% mutate(`Raw expenditure`= GDP*Population*(`Military Exp`/100)) %>% group_by(year) %>% 
  dplyr::mutate(`Mean expenditure (%)`=mean(`Military Exp`)) %>% 
  dplyr::mutate(`Mean raw expenditure ($)`=mean(`Raw expenditure`)) %>% 
  dplyr::mutate(`Mean raw difference`=`Raw expenditure`-`Mean raw expenditure ($)`) %>% 
  dplyr::mutate('Mean pctg difference'=`Military Exp`-`Mean expenditure (%)`) %>% 
  ungroup()

milexp$`Raw expenditure lag` <- ifelse(milexp$country==lag(milexp$country)&milexp$year==lag(milexp$year)+1,
                     milexp$`Raw expenditure`-lag(milexp$`Raw expenditure`),
                     0)

#Looking at heat graphs of deviance from mean based on relative and raw spending on military
ordered <- milexp %>% group_by(year) %>% top_n(20, `Raw expenditure`) %>% ungroup()

ordered$Status <- case_when(
  ordered$`Raw expenditure lag`>0 ~"Increased",
  ordered$`Raw expenditure lag`<0~"Decreased",
  ordered$`Raw expenditure lag`==0 ~ "No change",
  ordered$`Raw expenditure lag`== NA ~"No change"
)
ordered <- ordered[complete.cases(ordered),]

?scale_color_manual
#Look at changes over the years in the top countries form 1960 till 2017
cols <- c('Increased'="aquamarine", "Decreased"="light coral", "No change"="gray91")
movingdots <- ggplot(ordered, aes(country, `Raw expenditure`, color = factor(Status))) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = cols) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #animation setting
  labs(
    title = "Year: {frame_time}",
    x = "Country", 
    y = "Military Expenditure in USD"
  ) +
  transition_time(as.integer(year)) + #show the Year as integer
  shadow_mark(alpha = 0.3, size = 0.5) +
  ease_aes("linear")
animate(movingdots, width= 1800, height=600)


#Visualize the same information as bar chart
ordered_tidy <- ordered %>% group_by(year) %>% 
  mutate(rank = dense_rank(desc(`Raw expenditure`))) %>% ungroup()


ordered_formatted <- ordered_tidy %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-`Raw expenditure`),
         Value_rel = `Raw expenditure`/`Raw expenditure`[rank==1],
         Value_lbl = paste0(" ",round(`Raw expenditure`/1e9))) %>%
  group_by(country) %>% 
  ungroup()
  
#then do the graph
staticplot = ggplot(ordered_formatted, aes(rank, group = country, 
                                       fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = `Raw expenditure`/2,
                height = `Raw expenditure`,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=`Raw expenditure`,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))
#animate this
anim = staticplot + transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Top military expenditure per year : {closest_state}',  
       subtitle  =  "Top 20 Countries",
       caption  = "Military expenditure per year | Data Source: World Bank Data")

animate(anim, 200, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim2.gif"))
