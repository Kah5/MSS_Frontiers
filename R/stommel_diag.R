library(tidyverse)
library(ggplot2)


# read in lit review sheet (I downloaded, but could set this up to interface with the google drive)
lit.rev <- read.csv("data/Frontiers Lit Review Spreadsheet - Data.csv")
# these column names got converted with a lot of periods:
colnames(lit.rev) <- c("Date.found", "Authors", "Year", "Source",
                       "Title", "Keywords", "Location", "Mechanism","Mech.short", "Geographic.scale",
                       "Time.scale", "direction.tipping", "climate.direction", "proposed.solutions", 
                       "notes", "GIS.location")

# we want the mechanism, geographic, and temperal scale 

lit.rev %>% group_by(Geographic.scale)%>% summarise (n())

# will need to reclassify theses to put them on the diagram:
unique(lit.rev$Geographic.scale)
unique(lit.rev$Time.scale)

head(lit.rev)
#log distance:

# 0-1 = individual
# 0-3 = patch
# 3-5 = landscape
# 4-7 = region
# 7-8 = biosphere


# note that if we use this figure we need to think more about these scales....
# convert regional scale to a range of numbers:
geo.scale <- data.frame(Geographic.scale = c("Varied", "Continental", "Nationwide","0.5 x 0.5 scale", "Regional",
                                "regional", "Regional (1.8 mil acres)", "Coastal regions", "Multi Region",
                                "Small forest plots", "Small Fields", "local", "Smaller scale",
                                "Tree stands", "Coastal islands", "One island"), 
           meter.scale.min = c(NA,10^18, 10^12,10^-1, 10^3,10^3, 10^8, 10^2, 10^8,10^1,10^1,10^1,10^1,10^-1, 10^2, 10^2), 
           meter.scale.max = c(NA,10^20, 10^18, 10^1,10^8,10^8, 10^9, 10^7, 10^14,10^2,10^2,10^2,10^2,10^1, 10^4, 10^4))


# note that if we use this figure we need to think more about these scales....
# generate timsecale df too:
year.scale <- data.frame(Time.scale = unique(lit.rev$Time.scale), 
                         time.scale.min =c(NA,NA, 150, 50, 4, 25, 580, 40, 190,10, 1, 1,0.1,1,0.1,
                                           0.9,NA, 100, 1, 6500,50,1900, 4,13000, 1, 67, 0.1, 30, 15,NA), 
                         time.scale.max = c(NA, NA, 500, 100,6,30,1440, 50,210,100,10, 50,1,3,5,
                                            1.2,NA, 1000, 100,7500,200, 2100,6, 15000,10,100, 5, 50,20, NA))


lit.rev2 <- merge(lit.rev, geo.scale, by = "Geographic.scale")
lit.rev.recode <- merge(lit.rev2, year.scale, by = "Time.scale")


geo.time <- lit.rev.recode[,c("Mechanism", "Mech.short","time.scale.min", "time.scale.max", "meter.scale.min", "meter.scale.max", "Geographic.scale")]

geo.time$log.time.min <- log(geo.time$time.scale.min)
geo.time$log.time.max <- log(geo.time$time.scale.max)

geo.time$log.geo.min <- log(geo.time$meter.scale.min)
geo.time$log.geo.max <- log(geo.time$meter.scale.max)

ggplot()+
  geom_rect(data=geo.time, mapping=aes(xmin=log.time.min, xmax= log.time.max, ymin=log.geo.min, ymax=log.geo.max, fill=Mech.short, linejoin = "bevel"), color="black", alpha=0.75)+
  ylab("Spatial Scale (log)")+xlab("Time Scale (log)")#+scale_fill_manual(values = colorscheme)


#ggplot()+
 # geom_rect(data=geo.time, mapping=aes(xmin=time.scale.min, xmax= time.scale.max, ymin=meter.scale.min, ymax=meter.scale.max, fill=Mech.short), color="black", alpha=0.75, linejoin = "round")+
  #ylab("Spatial Scale (log)")+xlab("Time Scale (log)")+scale_fill_manual(values = colorscheme)

ggplot() + geom_histogram(data = geo.time, mapping = aes(time.scale.max)) + facet_wrap(~Mech.short, scales = "free_x")


ggplot(data=geo.time, mapping=aes(x=Geographic.scale,  y= log.time.max, fill=Mech.short))+
  geom_bar( stat= "identity", color="black", alpha=0.75, position = "dodge")+
  ylab("Time Scale (log)")+xlab("Time Scale (log)")+theme(axis.text.x = element_text(angle= 45, hjust = 1))#+scale_fill_manual(values = colorscheme)

unique(geo.time$Mech.short)

#d73027
#f46d43
#fdae61
#fee08b
#ffffbf
#d9ef8b
#a6d96a
#66bd63
#1a9850


colorscheme <- c(
"Fire-Climate" = "Purple",
"Fire" = "#d73027",
"Fire-Herbivore" = "#feb24c",
"Fire-Climate-Soils" = "#fee08b",
"Land-Use" = "grey",
"Climate" = "blue",
"Vegetation-Climate" = "#66bd63",
"Nutrient Subsidy" = "pink",
"Nutrient Subsidy-Herbivore" = "pink",
"Ecosystem Engineer" = "pink",
"Insect-Fire" = "brown", 
"Vegetation" = "#1a9850",
"Soil" = "#8c510a",
"Soils" = "#8c510a",
"Herbivore-Climate" = "#a8ddb5", 
"Herbivore" = "yellow",
"Livestock/Herbivore Grazing" = "yellow",
"NA" = "white", 
" " = "white"
)


# hypothesized stommel diagram:

dummy.data <- data.frame(Mech = c("Fire", "Climate", "Microclimate","Soils", "Herbivore", 
                    "Fire-Climate", "Fire-Herbivore", "Vegetation", "Fire-Climate-Soils", "Herbivore-Climate"),
           time.scale.min = c(1, 15, 1, 1, 1, 1, 10, 30, 25, 30),
           time.scale.max = c(10, 1000, 5,50, 20, 50, 50, 300, 500, 400),
           spatial.scale.min = c(1,15, 1, 1, 1, 75, 10, 1, 300,100), 
           spatial.scale.max = c(100, 1000,5, 20, 50, 1000, 100, 20,1000, 500))


ggplot()+
  geom_rect(data=dummy.data, mapping=aes(xmin=time.scale.min, xmax= time.scale.max, ymin=spatial.scale.min, ymax=spatial.scale.max, fill=Mech, linejoin = "bevel"), color="black", alpha=0.75)+
  ylab("Spatial Scale (log)")+xlab("Time Scale (log)")+scale_fill_manual(values = colorscheme)+scale_x_log10()+scale_y_log10()
