library(tidyverse)
install.packages("ggrepel")
install.packages("extrafont")
library(ggrepel)
library(RColorBrewer)
library(dplyr)
library(extrafont)
library(RCurl)
font_import() ## Press y in the console to import fonts, and wait for it to finish
pal <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", 
         "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", 
         "#CAB2D6","#6A3D9A", "#E9E909", "#B15928") 

#The dataset I will be using for the visualisation
#x <- getURL("https://raw.githubusercontent.com/abhinavsingh101/cs710_final/main/coffee_data_i.csv")
#y <- getURL("https://raw.githubusercontent.com/abhinavsingh101/cs710_final/main/coffee_data_regions50.csv")

#df <- read.csv(text = x)

##Which countries the major coffee varieties come from
df <- read.csv('coffee_data_i.csv')

{
  df$variety <- as.factor(df$variety)
  
  variety <- df%>%
    group_by(variety, country_of_origin) %>%
    summarise(vol=sum(volume),
              cup_pts=(median(total_cup_points))) %>%
    filter((variety == 'Caturra') | (variety == 'Bourbon') | (variety == 'Catuai') | (variety == 'SL14') | (variety == 'SL28') | (variety == 'SL34')) %>%
    filter((country_of_origin == 'Brazil') | (country_of_origin == 'Colombia') | 
             (country_of_origin == 'Costa Rica') | (country_of_origin == 'El Salvador') | 
             (country_of_origin == 'Ethiopia') | (country_of_origin == 'Guatemala') | 
             (country_of_origin == 'Honduras') | (country_of_origin == 'Kenya') | 
             (country_of_origin == 'Mexico') | (country_of_origin == 'Nicaragua') | 
             (country_of_origin == 'Tanzania, United Republic Of') | (country_of_origin == 'Uganda')) %>%
    ggplot(mapping = aes(x = variety, y = cup_pts, color = country_of_origin, size = vol, alpha = 0.9)) +
    geom_jitter(width=0.2) +
    geom_label_repel(aes(label = ifelse(vol>1000000 | cup_pts > 83 ,as.character(country_of_origin),'')),
                     min.segment.length = Inf,
                     box.padding   = 0.5,
                     point.padding = 0,
                     size = 5,
                     alpha=0.9,
                     max.overlaps=Inf,
                     show.legend = FALSE
    ) +
    coord_cartesian(ylim = c(80, 86)) +
    scale_size(range = c(8, 25)) +
    guides(colour = guide_legend(title='Country of origin', override.aes = list(size=8, alpha=0.9)),
           alpha=FALSE, size = guide_legend(title = 'Total volume')) +
    #scale_colour_brewer(palette = "Paired") +
    scale_color_manual(values=pal) +
    labs(title="Which countries the major coffee varieties come from",
         x ="Major Coffee Varieties", y = "Cupping points") +
    theme(plot.title=element_text(size=25, face="bold",family="Avenir"),
          axis.title=element_text(size=20,face="bold",family="Avenir"),
          axis.text=element_text(size=20,family="Avenir"),
          panel.background = element_rect(
            fill = "white",
#            fill = '#F3F2EE',
            colour = "grey50")
    )+
    #  theme(plot.margin = margin(t=4,1,1,1, "lines"))+
    #  theme(legend.direction = "horizontal") +
    theme(legend.position = "none")
  
  variety
}
ggsave(variety,filename = "variety.png", width = 14, height = 8, dpi = 320,
       device = "jpeg", path = "/Users/abhinavsingh/Downloads/CS_710/Practice R project/Core/Coffee/", units = "in")  

##The best coffee producing companies (by country)
df <- read.csv('coffee_data_i.csv')
{
  top_producers_df <- df%>%
    group_by(producer, region, country_of_origin, variety, processing_method)%>%
    summarise(vol = sum(volume),
              cupping_points=median(total_cup_points)
    ) %>%
    filter(vol>100000 & cupping_points > 80)
  
  top_producers_df$variety=ifelse(is.na(top_producers_df$variety),"Missing",top_producers_df$variety)
  top_producers_df$processing_method = ifelse(is.na(top_producers_df$processing_method),"Missing",top_producers_df$processing_method)
  
  producers<- top_producers_df %>%
    ggplot(aes(size=vol, x = country_of_origin, y=cupping_points, 
               color=variety, shape=processing_method)) +
    geom_jitter(alpha=0.7, width=0.2) +
    scale_shape_manual(values=c('Natural / Dry'= 16, 'Missing'=18, 'Washed / Wet' = 17)) +
    scale_color_manual(values=c('Bourbon' = "#E41A1C", 'SL28' = "#FF7F00", 'SL14' = "#A65628",
                                'Caturra' = '#984EA3', 'Other' = "#4DAF4A", 'Missing' = "#F781BF")) +
    scale_size(range = c(10, 25))+
    geom_label_repel(aes(label = ifelse(vol>1000000 | cupping_points > 82.5 ,as.character(producer), "")),
                     min.segment.length = Inf,
                     box.padding   = 0.2,
                     point.padding = 0.2,
                     size = 4,
                     alpha=0.8,
                     max.overlaps=Inf,
                     show.legend = Inf)+
    labs(title="The best coffee producing companies in the world (by country)",
         subtitle = "along with their variety, processing methods and sales volume",
         x ="Country of origin", y = "Cupping points")+
    guides(colour = guide_legend(title = 'Coffee variety', override.aes = list(size=8, alpha=0.8)), alpha=FALSE, 
           size = guide_legend(title = 'Total volume'),
           shape = guide_legend(title = 'Processing method', override.aes = list(size=8))) +
    theme(plot.title=element_text(size=26, face="bold",family="Avenir"),
          plot.subtitle=element_text(size=23,family="Avenir"),
          axis.title=element_text(size=21,face="bold",family="Avenir"),
          axis.text=element_text(size=21,family="Avenir"),
          panel.background = element_rect(
            fill = 'white',
            colour = "grey50"),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.box="vertical", legend.margin=margin(),
          legend.title=element_text(size=15,family="Avenir"))+
    coord_cartesian(ylim = c(80, 86))
  producers
}
ggsave(producers,filename = "producers.png", width = 16.1, height = 11, dpi = 320,
       device = "jpeg", path = "/Users/abhinavsingh/Downloads/CS_710/Practice R project/Core/Coffee/", units = "in")

#Countries with the best coffee (Ethiopia wins)
df <- read.csv('coffee_data_i.csv')
{  

  df$best_countries <- 0
  df$best_countries <- ifelse((df$country_of_origin == 'Ethiopia' & 
                                 (df$country_of_origin != 'Kenya' & 
                                    df$country_of_origin != 'Costa Rica' & 
                                    df$country_of_origin != 'Uganda')), 4, df$best_countries)
  df$best_countries <- ifelse((df$country_of_origin == 'Kenya' & 
                                 (df$country_of_origin != 'Ethiopia' & 
                                    df$country_of_origin != 'Costa Rica' &
                                    df$country_of_origin != 'Uganda')), 3, df$best_countries)
  df$best_countries <- ifelse((df$country_of_origin == 'Costa Rica' & 
                                 (df$country_of_origin != 'Kenya' & 
                                    df$country_of_origin != 'Ethiopia' &
                                    df$country_of_origin != 'Uganda')), 1, df$best_countries)
  df$best_countries <- ifelse((df$country_of_origin == 'Uganda' & 
                                 (df$country_of_origin != 'Kenya' & 
                                    df$country_of_origin != 'Costa Rica' &
                                    df$country_of_origin != 'Ethiopia')), 2, df$best_countries)
  
  
  df$best_countries <- as.factor(df$best_countries)
  
  best_countries <- ggplot(data=subset(df, !is.na(best_countries)), 
                           aes(x=best_countries, y=total_cup_points, 
                               fill = best_countries )) +
    geom_violin()+
    scale_y_continuous(name="Cupping points", limits=c(70,90),
                       breaks = c(70, 75, 80, 85, 90))+
    
    scale_x_discrete(labels = c("Remaining \ncountries", "Costa Rica", "Uganda", "Kenya", "Ethiopia"))+
    stat_summary(fun=mean, geom="point", size=3, color="grey") +
    labs(title="Ethiopian coffee is better \nthan the rest",
         y = "Cupping points")+
    
    theme(plot.title=element_text(size=25, face="bold",family="Avenir"),
          axis.title=element_text(size=25,face="bold",family="Avenir"),
          axis.text=element_text(size=22,family="Avenir"),
          axis.title.y = element_blank(),
          panel.background = element_rect(
            fill = 'white',
#            fill = '#e7f4ed',
            colour = "grey50"),
          legend.position = "none")+
    scale_fill_brewer(palette="Blues")+
    coord_flip()
  
  best_countries
}

ggsave(best_countries,filename = "best_countries.png", width = 7.5, height = 12, dpi = 320,
       device = "jpeg", path = "/Users/abhinavsingh/Downloads/CS_710/Practice R project/Core/Coffee/", units = "in")  


#Does the colour of the beans affect quality? 
df <- read.csv('coffee_data_i.csv')
{
df_color<- df %>% 
  mutate(total_cup_points_std = (total_cup_points - mean(total_cup_points))/sd(total_cup_points))
df$color <- as.factor(df$color)
d_color <- ggplot(data=subset(df_color, !is.na(color)),
                  aes(x = color, y=total_cup_points_std, color=color)) + geom_jitter(alpha=0.8, size=2) +
  #  scale_colour_manual(values = c('#2596be','#01f8ba','#13b35e','#a6cee3')) +
  #    scale_y_continuous(name="Cupping points", limits=c(70,95),
  #                       breaks = c(75, 90))
  scale_y_continuous(name="Cupping points", limits=c(-2,2),
                     breaks = c(-1.8,1.8),
                     labels = c("-1.8" = "Less", "1.8" = "More"))

d_color <- d_color + scale_x_discrete(limits = c("Green", "Bluish-Green", 
                                                 "Blue-Green", 
                                                 "None")) + 
  stat_summary(geom = "point", fun = "median", colour = "black", size = 4) +
  coord_flip() +
  theme(axis.text=element_text(size=25,family="Avenir"),
        axis.title=element_text(size=28,face="bold",family="Avenir"),
        plot.title= element_text(size=25, face='bold',family="Avenir"),
        legend.position = "none",
        axis.title.y = element_blank(),
        panel.background = element_rect(
          fill = 'white',
          colour = "grey50")
  ) + 
  labs(title = "Colour has no relationship \nwith cupping points")

d_color
}
ggsave(d_color ,filename = "color_h.png", width = 12, height = 3, dpi = 320,
       device = "jpeg", path = "/Users/abhinavsingh/Downloads/CS_710/Practice R project/Core/Coffee/", units = "in")  

ggsave(d_color ,filename = "color_v.png", width = 8, height = 12, dpi = 320,
       device = "jpeg", path = "/Users/abhinavsingh/Downloads/CS_710/Practice R project/Core/Coffee/", units = "in")  


#Does processing method affect coffee?
df <- read.csv('coffee_data_i.csv')
{
  df<- df %>% 
    mutate(total_cup_points_std = (total_cup_points - mean(total_cup_points))/sd(total_cup_points))
  
  
  processing_methods <- c("Washed / Wet" = "Washed /\n Wet", "Natural / Dry" = "Natural /\n Dry", 
                          "Semi-washed / Semi-pulped" = "Semi-washed /\n Semi-pulped", 
                          "Pulped natural / honey" = "Pulped natural /\n honey", "Other" = "Other")
  
  df$processing_methods_clean <- as.factor(processing_methods[df$processing_method])
  
  #df$processing_methods_clean <- as.factor(df$processing_methods_clean)
  #df$processing_methods_clean
  
  processing <- ggplot(data=subset(df, !is.na(processing_methods_clean)),
                       aes(x = processing_methods_clean, y=total_cup_points_std, 
                           color=processing_methods_clean))+
    geom_jitter(size=2, alpha=0.5)+
    scale_y_continuous(name="Cupping Points", limits=c(-2,2), 
                       breaks = c(-1.8, 1.8),
                       labels = c("-1.8" = "Less","1.8" = "More"))
  
  processing <- processing + scale_x_discrete(limits = c("Washed /\n Wet", "Natural /\n Dry", 
                                                         "Semi-washed /\n Semi-pulped", 
                                                         "Pulped natural /\n honey", "Other")) + 
    stat_summary(geom = "point", fun = "median", colour = "black", size = 4) +
    coord_flip() +
    theme(axis.text=element_text(size=22,family="Avenir"),
          axis.title=element_text(size=25,face="bold",family="Avenir"),
          plot.title= element_text(size=25, face='bold',family="Avenir"),
          legend.position = "none",
          axis.title.y = element_blank(),
          panel.background = element_rect(
            #fill = '#e7f4ed', 
            #fill = '#dfe4df',
            fill = 'white',
            colour = "grey50")) +
    labs(title = "Processing method has no \nrelationship with cupping points",
         x = "Cupping Points") +
    scale_colour_brewer(palette = "Set1")
  
  processing
}
ggsave(processing,filename = "processing_h.png", width = 12, height = 3, dpi = 320,
       device = "jpeg", path = "/Users/abhinavsingh/Downloads/CS_710/Practice R project/Core/Coffee/", units = "in")  

ggsave(processing,filename = "processing_v.png", width = 8, height = 12, dpi = 320,
       device = "jpeg", path = "/Users/abhinavsingh/Downloads/CS_710/Practice R project/Core/Coffee/", units = "in")  

#ALTITUDE

#y <- getURL("https://raw.githubusercontent.com/abhinavsingh101/cs710_final/main/coffee_data_regions50.csv")
#df_2 <- read.csv(text = y)
df_2 <- read_csv('coffee_data_regions50.csv')

{df_2 <- df_2 %>%
  filter(country_of_origin != 'India')
top_region_country<- df_2 %>% 
    count(region, country_of_origin) %>%
    group_by(region)%>%
    slice(which.max(n))
  
  # creating a nummeric ID for each country
  top_region_country$id[top_region_country$country_of_origin=='Tanzania, United Republic Of'] <-14
  top_region_country$id[top_region_country$country_of_origin=='Kenya']<-19
  top_region_country$id[top_region_country$country_of_origin=='Costa Rica']<-16
  top_region_country$id[top_region_country$country_of_origin=='Guatemala']<-17
  top_region_country$id[top_region_country$country_of_origin=='Mexico']<-18
  top_region_country$id[top_region_country$country_of_origin=='Ethiopia']<-15
  top_region_country$id[top_region_country$country_of_origin=='Uganda']<-20
  top_region_country$id[top_region_country$country_of_origin=='Colombia']<-21
  top_region_country$id[top_region_country$country_of_origin=='Brazil'] <-22
  top_region_country$id[top_region_country$country_of_origin=='Honduras']<-23
  top_region_country$id[top_region_country$country_of_origin=='Nicaragua']<-24
  top_region_country$id[top_region_country$country_of_origin=='El Salvador']<-25
  
  df_top_regions <- df_2 %>%
    group_by(region) %>%
    summarise(cupping_points=median(total_cup_points),
              total_volume=sum(volume),
              altitude = median(altitude_mean_meters),
              na.rm = TRUE)
  
  df_countries <- merge(df_top_regions, top_region_country, by='region')
  df_countries$country_of_origin <- as.factor(top_region_country$country_of_origin)
  
  d1<- ggplot(df_countries, 
              aes(y=cupping_points, x=altitude,
                  size=total_volume,
                  alpha=0.8,
                  colour=country_of_origin),
              key_glyph = "point") +
    geom_point(key_glyph = "point") + 
    geom_label_repel(aes(label = ifelse(total_volume>1400000 | cupping_points > 85 ,as.character(region),'')),
                     min.segment.length = Inf,
                     box.padding   = 0.55,
                     point.padding = 0.25,
                     size = 5,
                     alpha=0.9,
                     max.overlaps=Inf,
                     show.legend = FALSE
    ) +
    labs(title="How the altitude at which coffee grows affects its quality", 
         subtitle = "and which regions and countries stand out in quality and volumes",
         y="Cupping points",
         x="Altitude (meters)")+
    scale_y_continuous(name="Cupping points", limits=c(80,90),
                       breaks = c(80, 82, 84, 86, 88, 90))

#    theme(plot.title=element_text(size=17, face="bold"),
#          axis.title=element_text(size=15,face="bold"),
#          axis.text=element_text(size=15))
  
  altitude <- d1 + scale_size(range = c(8, 25)) +
    guides(
      colour = guide_legend(
        title = 'Country \nof origin', 
        override.aes = list(size=8, alpha=0.9)), 
      alpha=FALSE, 
      size = guide_legend(title = 'Total volume')) +
    theme(
      axis.text=element_text(size=20,family="Avenir"),
      axis.title=element_text(size=20,face="bold",family="Avenir"),
      plot.title= element_text(size=25, face='bold',family="Avenir"),
      plot.subtitle= element_text(size=20,family="Avenir"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title=element_text(size=15,family="Avenir"),
      panel.background = element_rect(
        fill = 'white',
        colour = "grey50")
    )+
    scale_color_manual(values=pal) +
#    scale_colour_brewer(palette = "Paired") +
    scale_x_continuous(limits = c(800, 2500))
  
  altitude
  }
ggsave(altitude,filename = "altitude.png", width = 15.1, height = 10, dpi = 320,
       device = "jpeg", path = "/Users/abhinavsingh/Downloads/CS_710/Practice R project/Core/Coffee/", units = "in")  

