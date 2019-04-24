## STI Testing: ART-Net Data Map ##
rm(list = ls())
library(haven)
library(Hmisc)
library(zipcode)
library(magrittr)
library(sf)
library(rgdal)
library(tidyverse)
library(maps)
library(viridis)
library(albersusa)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
#devtools::install_github('arilamstein/choroplethrZip@v1.3.0')
library(choroplethrZip)


# Read in datasets ---------
data(zipcode)
us <- map_data('state')
zip.shape <- readOGR(dsn = "Input",
                     layer = "tl_2018_us_zcta510",
                     stringsAsFactors = FALSE)
artnet <- readRDS("Output/artnet4shiny.rda")

# Clean ART-Net ZIPs
artnet$ZIPCODE2 <- clean.zipcodes(artnet$ZIPCODE2)
artnet <- rename(artnet, zip = ZIPCODE2)

# Aggregate mean deg by ZIP
a <- aggregate(artnet$totdegree, by = list(artnet$zip), FUN = mean, na.rm = TRUE)
colnames(a) <- c("zip", "meandeg")
a$zip <- as.character(a$zip)
# 453 NaNs

# Merge with ZIP Code data file
merged <- left_join(zipcode, a, by = c('zip' = 'zip'))
merged$meandeg[which(is.nan(merged$meandeg))] <- -1
merged$meandeg[which(is.na(merged$meandeg))] <- -1
merged <- ggplot2::fortify(merged, region = 'zip')


# Spatial/mapping ---------------------

#to avoid upload error_FM - did not work, changed dsn and added 'layer':
fortified.zip.shape <- fortify(zip.shape,
                                    region = "ZCTA5CE10")

# Merge shape file to survey data
# merge fortified shape data with prevalence data
fortified.zips.shape <- left_join(fortified.zip.shape,
                                      merged,
                                      by = c("ZCTA5CE10" = "zip"))

# plot using ggplot
m1 <- ggplot() +

  # plot counties (shade using "value" from prevalence data) and clinics (currently, all clinics)
  ## can add to aes the option color (inside the ()) to color points by group (type of service)
  geom_polygon(data = fortified.zips.shape,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = meandeg),
               color = "gray78", size = .35)
  # scale_colour_manual(name = "",
  #                     values = c("0" = "black","1" = "red"),
  #                     labels=c("All substance use treatment services","Substance use services that accept
  #                              Medicaid, and serve LGBT* Clients")) +
  #
  # #legend categories and labels - \n = new line
  # scale_fill_manual("Prevalent HIV Infections, 2015 \nCases per 1,000 population",
  #                   values = c("(-4,50]"= "#bbdadc", "(50,100]"= "#8dc1c5",
  #                              "(100,300]"= "#55a2a8", "(300,2.5e+03]"="#396c70"),
  #                   labels=c("< or = 0.5", "0.5 - 1", "1 - 3", "> or = 3"))

# add state borders
m2 <- m1 + geom_polygon(data = us,
                        aes(x = long,
                            y = lat,
                            group = group),
                        color = 'gray',
                        fill = NA,
                        alpha = 0.35) +
  theme_void()


# view map
m3 <- m2 + labs(caption="*LGBT: Lesbian, Gay, Bisexual,Transgender
                Note: The substance use treatment services displayed here include only the ones listed in the SAMHSA dataset.
                The 9 deep south states included in the map are: Alabama, Florida, Georgia, Louisiana, Mississippi, North Carolina, South Carolina, Tennessee, and Texas.
                Sources: United States Department of Health and Human Services. Substance Abuse and Mental Health Services Administration (SAMHSA). Center for Behavioral Health Statistics and Quality. National Survey on Drug Use and Health, 2016.
                Available at: samhsa.gov (accessed on 10/02/2018).
                AIDSVu. Emory University, Rollins School of Public Health. Available at: aidsvu.org (accessed on 10/02/2018).")
m4  <- m3 + ggtitle("Substance use treatment locations that accept Medicaid, and serve LGBT* Clients")

m5  <- m4 + theme(plot.title = element_text(size = 13,
                                            face = "bold",
                                            hjust = 0.5,
                                            vjust = 1,
                                            lineheight = 1,
                                            margin = margin(0,0,10,0)),
                  panel.border = element_rect(colour = "black",
                                              fill = NA,
                                              size = 1),
                  plot.caption = element_text(size = 7,
                                              vjust = 0,
                                              hjust = 0,
                                              lineheight = 1), #lineheight: space between title lines
                  legend.title = element_text(colour = "black",
                                              size = 7,
                                              face = "bold"),
                  plot.margin = unit(c(0.1, 0.1, 0.1, 0.1),
                                     "cm")) #top, right, bottom, left


m6 <- m5 +  ggsn::scalebar(us, dist = 250,
                          st.size = 3,
                          height = 0.02,
                          dd2km = TRUE,
                          model = 'WGS84',
                          location = "bottomleft")

m7  <- m6 + north(data = NULL,
                  location = "bottomright",
                  symbol = 1,
                  scale = 0.15,
                  x.min = -110,
                  x.max = -73,
                  y.min = 24,
                  y.max = 37,
                  anchor = NULL)

#changing scale to be the same in x and y (instead of projection)
m8 <- m7 + coord_fixed(ratio = 1.2,
                           xlim = NULL,
                           ylim = NULL,
                           expand = TRUE,
                           clip = "on")


# RSTudio pub --------------
# https://rstudio-pubs-static.s3.amazonaws.com/274683_cd798195cbaf4a7a900cceaafb73ea11.html
#
#
# Hos$county<-tolower(Hos$county)
# choropleth <- merge(county_df, Hos, by = c("state", "county"))
# choropleth <- choropleth[order(choropleth$order), ]
# Upgrade to Heatmap
# #
# ggplot(choropleth, aes(long, lat, group = group)) +
#   geom_polygon(data=county_df,aes(long,lat,group=group),fill='#ffffb3',colour = alpha("gray", 1 / 2))+
#   geom_polygon(aes(fill = count), colour = alpha("gray", 1 / 2), size = 0.2) +
#   geom_polygon(data = state_df, colour = "white", fill = NA) +
#   theme(axis.line = element_blank(), axis.text = element_blank(),
#         axis.ticks = element_blank(), axis.title = element_blank()) +
#   scale_fill_gradientn(colours = rev(rainbow(7)),
#                        breaks = c(0:6),
#                        trans = "log10") +theme_minimal()







fm.counties<-aggregate(fm$count,by=list(fm$county,fm$state),sum)
names(fm.counties)[1:2]<-c('county','state')
cty_sf <- counties_sf("aeqd")
cty_sf$county<-as.character(cty_sf$name)
cty_sf$state<-as.character(cty_sf$iso_3166_2)
data.fm<-left_join(cty_sf,fm.counties,by=c('state','county'))
data.fm$x<-log(data.fm$x)
data.fm$x[is.na(data.fm$x)]<-0

data.fm %>%
  ggplot(aes(fill = x, color = x)) +
  geom_sf() +
  scale_fill_viridis(option = "B",direction=-1) +
  scale_color_viridis(option = "B",direction=-1) +
  theme_map(base_size=11)


# https://medium.com/@anjesh/step-by-step-choropleth-map-in-r-a-case-of-mapping-nepal-7f62a84078d9
# http://austinwehrwein.com/digital-humanities/creating-a-density-map-in-r-with-zipcodes/
# https://www.arilamstein.com/blog/2015/08/27/choroplethr-3-2-0-is-now-out/
# https://stackoverflow.com/questions/36415555/how-to-make-zip-code-based-maps-in-r
