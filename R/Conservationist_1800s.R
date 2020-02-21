library(sp)
library(rgdal)
library(sf)
library(foreign)


# read in the PADUS land use:

states <-readOGR("data/PADUS1_4MWest_Shapefile/tl_2014_us_state_AlbersUSGS.shp")
protected.areas <-readOGR("data/PADUS1_4MWest_Shapefile/PADUS1_4Fee_Easements_MWest.shp")
protected.areas <-readOGR("data/PADUS1_4MWest_Shapefile/")


# lots of DBF's
Des.tp<- read.dbf("data/PADUS1_4MWest_Shapefile/Des_Tp.dbf")
Category<- read.dbf("data/PADUS1_4MWest_Shapefile/Category.dbf")
state.dbh<- read.dbf("data/PADUS1_4MWest_Shapefile/tl_2014_us_state_AlbersUSGS.dbf")
mpa.dbf<- read.dbf("data/PADUS1_4MWest_Shapefile/PADUS1_4MPA.dbf")

iucn.dbf<- read.dbf("data/PADUS1_4MWest_Shapefile/IUCN_Cat.dbf")
access.dbf<- read.dbf("data/PADUS1_4MWest_Shapefile/Access.dbf")
state.dbf<- read.dbf("data/PADUS1_4MWest_Shapefile/State_Nm.dbf")
# calculate the areas in different types of land protection status



protected.areas$area <- st_area(f)



# total us protexted areas cover 14% of teh US...if this number is the same for 

# read in the pls and fia data:
library(ggplot2)
library(cowplot)

# read in density data
dens <- read.csv("data/density_full_FIA_PLS_unc.csv")


# read in the FIA & PLScomposition clusters
clust_plot10 <- read.csv("data/ten_clust_combined_dissimilarity_stat_smooth.dens.csv")
clust_plot_pls <- clust_plot10[clust_plot10$period %in% "PLS",]
# merge the clusters and pls density data: 
clust_10 <- merge(clust_plot_pls, dens, by = c("x", "y"))

# need to rename the clusters here (should go back and do it in the place where we originally make the clusters):
head(clust_10)
library(plyr)
clust_10$foresttype <- revalue(clust_10$speciescluster, c("Poplar/Oak-FIA"="Aspen", 
                                                          "Oak/Maple/Ash/Poplar-FIA"="Oak/Maple/Ash", 
                                                          "Oak-PLS" = "Oak", 
                                                          "Hemlock/Cedar/Maple-PLS" = "N. Mixed Forest", 
                                                          "Oak/Hickory/Elm/Maple-FIA" = "Oak-Hickory",
                                                          "Pine/Maple/Poplar/Oak/Ash-FIA" = "Pine", 
                                                          "Poplar/Pine/Tamarack/Fir-PLS" = "Boreal/Sub-boreal", 
                                                          "Beech/Maple/Pine-PLS" = "Beech-Maple",
                                                          "Maple/Cedar/Poplar-FIA" = "Maple Mixed Forest",
                                                          "Oak/Maple/Other/Hickory-FIA" = "Oak-Mixed"))

clust_10$orderedforesttype <- factor(clust_10$foresttype, c("Oak", "Pine", "Aspen", "N. Mixed Forest", "Boreal/Sub-boreal","Oak/Maple/Ash", "Oak-Hickory", "Beech-Maple", "Maple Mixed Forest", "Oak-Mixed"))
#clust_10$orderedforesttype <- factor(dens.clust$foresttype, levels = rev(c("Aspen", "Maple Mixed Forest", "Oak-Mixed","Oak/Maple/Ash","Oak-Hickory","Pine","Oak",  "Boreal/Sub-boreal", "N. Mixed Forest",  "Beech-Maple")))

# create a stable coloring scheme:
compColors <- c('#386cb0',"#f0027f",'#ff7f00',"#ffff99","#7fc97f", "#beaed4",'#a6cee3',"#b15928",  "#004529",  '#fdc086')
names(compColors) <- levels(clust_10$orderedforesttype)


composition.map<- ggplot(clust_10, aes(x,y, fill = orderedforesttype))+geom_raster()+scale_fill_manual(values = compColors)+ theme_bw()


# okay lets say we can only save 14% of the Midwest, or 1649.76 grid cells:

# 0.14*11784
# [1] 1649.76


#---------------------How might we divy up the land conserved if we wanted:------------------------
# we can keep 1650 grid cells:

ncell.bycomp <- clust_10 %>% group_by(orderedforesttype) %>% summarise(ntype = n())

# maximum # of different community types
# we want an equal area of all forest types, we can save 165 grid cells for each community type:
ncells.tosave <- 1650/length(unique(ncell.bycomp$orderedforesttype)) 
ncell.bycomp$pct.equal.area <- (ncells.tosave/ncell.bycomp$ntype)*100
ncell.bycomp$equal.area <- 1650/length(unique(ncell.bycomp$orderedforesttype)) 
# save land that is proportional to the total area covered by composition type:
ncell.bycomp$porportion.comp <- 1650*(ncell.bycomp$ntype/sum(ncell.bycomp$ntype))

# prioritizing the densest places:

density.pls <- read.csv("data/PLS_mean_density_by_taxa.csv")
density.pls$x <- as.numeric(as.character(density.pls$x))

ggplot(density.pls, aes(x,y, fill = Total))+geom_raster()
top.density <- density.pls %>% top_n(wt = Total, 1650)

top.density$top.14.density <- "Top14.density"

clust_10.density <- merge(clust_10, top.density[,c("x", "y", "Total", "top.14.density")], by = c("x", "y"), all.x = TRUE)

ncell.high.density <- clust_10.density  %>% group_by(orderedforesttype, top.14.density) %>% summarise(ntype = n())

nona.hig.density<- ncell.high.density[!is.na(ncell.high.density$top.14.density),]
colnames(nona.hig.density)[3] <- "Top14.density"

ncell.bycomp<- merge(ncell.bycomp, nona.hig.density, by = "orderedforesttype", all.x = TRUE)

# where are these high density forests located:
density.priority.map <- ggplot(clust_10.density, aes(x,y, fill = orderedforesttype))+geom_raster()+scale_fill_manual(values = compColors)+
  geom_point(aes(x,y, color = top.14.density), size = 0.1)+ theme_bw()+scale_color_manual(values = c("Top14.density"="black", `NA` = "grey"))+theme(legend.position = "none")


# prioritizing the  diveristy of trees
ggplot(density.pls, aes(x,y, fill = Total))+geom_raster()

count.pls <- density.pls
dens.melt <- reshape2::melt(density.pls, id.vars = c("x", "y"))
dens.melt$occupancy <- ifelse(dens.melt$value >=1, 1, 0) # if there is some density of trees, then 
dens.melt.2 <- dens.melt[!dens.melt$variable %in% "Total",]
ntaxa<- dens.melt.2 %>% group_by(x,y) %>% summarise(taxa = sum(occupancy))

ggplot(ntaxa, aes(x,y, fill = taxa))+geom_raster()
top.diversity <- ntaxa %>% ungroup() %>%  top_n(wt = taxa, 1650)

top.diversity$top.14.diversity <- "Topdiversity"
clust_10.diversity <- merge(clust_10, top.diversity[,c("x", "y", "taxa", "top.14.diversity")], by = c("x", "y"), all.x = TRUE)

ncell.high.diversity <- clust_10.diversity  %>% group_by(orderedforesttype, top.14.diversity) %>% summarise(ntype = n())

nona.hig.diversity<- ncell.high.diversity[!is.na(ncell.high.diversity$top.14.diversity),]
colnames(nona.hig.diversity)[3] <- "Top14.diversity"

ncell.bycomp<- merge(ncell.bycomp, nona.hig.diversity, by = "orderedforesttype", all.x = TRUE)


# Prioritizing Carbon storage (Maximum biomass)

biomass.pls <- read.csv("data/PLS_mean_biomass_by_taxa.csv")
biomass.pls$x <- as.numeric(as.character(biomass.pls$x))

ggplot(biomass.pls, aes(x,y, fill = Total))+geom_raster()
top.biomass <- biomass.pls %>% top_n(wt = Total, 1650)

top.biomass$top.14 <- "Top14.biomass"

clust_10.biomass <- merge(clust_10, top.biomass[,c("x", "y", "Total", "top.14")], by = c("x", "y"), all.x = TRUE)

ncell.high.biomass <- clust_10.biomass  %>% group_by(orderedforesttype, top.14) %>% summarise(ntype = n())

nona.hig.biomass<- ncell.high.biomass[!is.na(ncell.high.biomass$top.14),]
colnames(nona.hig.biomass)[3] <- "Top14.biomass"

ncell.bycomp <- merge(ncell.bycomp, nona.hig.biomass, by = "orderedforesttype", all.x = TRUE)

# where are these high biomass forests located:
biomass.priority.map <- ggplot(clust_10.biomass, aes(x,y, fill = orderedforesttype))+geom_raster()+scale_fill_manual(values = compColors)+
  geom_point(aes(x,y, color = top.14), size = 0.1)+ theme_bw()+scale_color_manual(values = c("Top14.biomass"="black", `NA` = "grey"))+theme(legend.position = "none")



# barplot of the total to save under each scernario:
ggplot(ncell.bycomp, aes(x = orderedforesttype, y = ntype))+geom_bar(stat = "identity")

ncells.strategies <- ncell.bycomp %>% select(-top.14.density, -top.14, -ntype, - pct.equal.area)
ncells.melt <- reshape2::melt(ncells.strategies, id.vars ="orderedforesttype")


png(height = 4, width = 8, units = "in", res = 200, "outputs/ngrid_to_preserve_by_strategy.png")
ggplot(ncells.melt, aes(x = orderedforesttype, y = value, fill= variable))+geom_bar(stat= "identity", position = "dodge")+theme_bw(base_size = 12)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ylab("# grid cells")+ xlab(" ")
dev.off()


comp.legend <- get_legend(composition.map)

png(height = 4, width = 10, units = "in", res = 200, "outputs/maps_comp_preserve_strategy.png")
plot_grid(plot_grid(composition.map+theme(legend.position = "none"),density.priority.map, biomass.priority.map, ncol = 3),
          comp.legend, ncol = 2, rel_widths = c(1, 0.25))
dev.off()

# barplot of the percent of each forest composition type to save under each scernario:

# what is actually left of these composition clusters:
clust_plot10 <- read.csv("data/ten_clust_combined_dissimilarity_stat_smooth.dens.csv")
clust_plot_fia <- clust_plot10[clust_plot10$period %in% "FIA",]
# merge the clusters and pls density data: 
clust_10_fia <- merge(clust_plot_fia, dens, by = c("x", "y"))

# use PAD data to select what is actually left of each composition cluster type
