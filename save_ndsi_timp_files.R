
library(dplyr);library(ggplot2)

rgpth = "/Users/mattolson/src/glac-gee/results"

fls = list.files(rgpth, pattern = "narrow", full.names = T)
tables = lapply(fls, function(x){ddf = read.csv(x);ddf$sensor = strsplit(basename(x), "_")[[1]][1];return(ddf)})
combined_df <- do.call(rbind, tables)

combined_df %>% 
  ggplot(aes(x=year, y = area_m2_min, colour=sensor)) +
  geom_line()

# combined_df %>% select(-c("date_closest", "area_m2_closest")) %>% 
#   write.csv(., file.path(rgpth, "ndsi_1985_2024_timp_full.csv"), row.names = F)



# fls = list.files(rgpth, pattern = "root", full.names = T)[-c(1,3,5,6)]
fls = list.files(rgpth, pattern = "root", full.names = T)[c(2,4)]
tables = lapply(fls, function(x){ddf = read.csv(x);ddf$sensor = strsplit(basename(x), "_")[[1]][1];return(ddf)})
combined_df_root <- do.call(rbind, tables)

combined_df_root %>% 
  ggplot(aes(x=year, y = area_m2_min, colour=sensor)) +
  geom_line()


combined_df_root %>% select(-c("date_closest", "area_m2_closest")) %>% 
  write.csv(., file.path(rgpth, "ndsi_1985_2024_timp_root.csv"), row.names = F)



#### GAD VALLEY



rgpth = "/Users/mattolson/src/glac-gee/results"

fls = list.files(rgpth, pattern = "gad.csv", full.names = T)
tables = lapply(fls, function(x){ddf = read.csv(x);ddf$sensor = strsplit(basename(x), "_")[[1]][1];return(ddf)})
combined_df <- do.call(rbind, tables)

combined_df %>% 
  ggplot(aes(x=year, y = area_m2_min, colour=sensor)) +
  geom_line()

# combined_df %>% select(-c("date_closest", "area_m2_closest")) %>%
#   write.csv(., file.path(rgpth, "ndsi_1985_2024_gad_full.csv"), row.names = F)

# root zone

fls = list.files(rgpth, pattern = "gad_root", full.names = T)[-3]
tables = lapply(fls, function(x){ddf = read.csv(x);ddf$sensor = strsplit(basename(x), "_")[[1]][1];return(ddf)})
combined_df_root_gad <- do.call(rbind, tables)

combined_df_root_gad %>% 
  ggplot(aes(x=year, y = area_m2_min, colour=sensor)) +
  geom_line()


# combined_df_root_gad %>% select(-c("date_closest", "area_m2_closest")) %>% 
#   write.csv(., file.path(rgpth, "ndsi_1985_2024_gad_root.csv"), row.names = F)


# pt2

rgpth = "/Users/mattolson/src/glac-gee/results"
fls = list.files(rgpth, pattern = "pt2", full.names = T)
tables = lapply(fls, function(x){ddf = read.csv(x);ddf$sensor = strsplit(basename(x), "_")[[1]][6];return(ddf)})
combined_df2 <- do.call(rbind, tables)


rgpth = "/Users/mattolson/src/glac-gee/results"
flspt = list.files(rgpth, pattern = "pt2", full.names = T)
dfn = read.csv(flspt[1])
dff = read.csv(flspt[2]) #;dff$pixel_count=dff$pixel_count*1.1
combined_df2 = merge(dfn, dff, by='year', all=T)
names(combined_df2) = c('year','no', 'filt')
reshape2::melt(combined_df2, id.var='year') %>% 
  ggplot(aes(x=year, y = value, colour=variable)) +
  geom_line()


merge(combined_df_root[c('year','area_m2_min')], combined_df2, by='year', all=T) %>% 
  mutate(root = area_m2_min/max(area_m2_min, na.rm=T)*5) %>% select(-area_m2_min) %>% 
  reshape2::melt(., id.var='year') %>% 
  ggplot(aes(x=year, y = value, colour=variable)) +
  geom_line()


dfpt2 = read.csv(list.files(rgpth, pattern = "pt2.csv", full.names = T))
df_miss = data.frame(year=c(1998, 2005, 2012, 2013, 2023), pixel_count=c(1,1,NA,NA, 1) )
dfc = rbind(dfpt2, df_miss)
dfc = dfc[match(sort(dfc$year), dfc$year),]

dfpt1 = read.csv(list.files(rgpth, pattern = "pt1.csv", full.names = T))
df_miss = data.frame(year=c(1998, 2005, 2012, 2013, 2023), pixel_count=c(1,1,NA,NA, 1) )
dfc1 = rbind(dfpt1, df_miss)
dfc1 = dfc1[match(sort(dfc1$year), dfc1$year),]



# these three must match up!
df00 = merge(merge(combined_df_root[c('year','area_m2_min')], dfc1, by='year', all=T), dfc, by='year', all=T)
names(df00) = c('year', 'sca_m2_rooting', 'pt1_snow', 'pt2_snow')

df00 %>% 
  mutate(rooting_zone = sca_m2_rooting/max(sca_m2_rooting, na.rm=T)) %>% select(-sca_m2_rooting) %>% 
  reshape2::melt(., id.var='year') %>% 
  ggplot(aes(x=year, y = value, colour=variable)) +
  geom_line()

df00$sca_m2_rooting[14] = 99000
df00$pt2_snow[39] = 1
#
df00 %>% 
  write.csv(., file.path(rgpth, "ndsi_timp_pts.csv"), row.names = F)



# 1998
b2 = rast("~/Downloads/LT05_L2SP_038032_19980904_20200908_02_T1_SR_B2.TIF")*(0.0000275)+(-0.2)
b5 = rast("~/Downloads/LT05_L2SP_038032_19980904_20200908_02_T1_SR_B5.TIF")*(0.0000275)+(-0.2)

ndsi = ((b2-b5) / (b2+b5)) %>% project(v) %>% 
  crop(v, mask=T) 

plot(ndsi>0.4)

sum(values((ndsi)>0.4), na.rm=T) / sum(!is.na(values(ndsi)))

sum(values((ndsi)>=0.4), na.rm=T)*0.3*0.3
sum(values((ndsi)>=0.4), na.rm=T)*30*30
# 9.9 km2 or 99000 m2

plot(project(pt1, v), add=T)
plot(project(pt2, v), add=T)











library(terra)
shpth = '~/src/glac-gee/shp/gad-rg/Gad_Rock_Glacier_Rooting_Zone.json'
shpth = '~/src/glac-gee/shp/gad-rg/Gad_Rock_Glacier_whole_RG.json'
v = vect(shpth)
plot(v)
# writeVector(v, file.path(dirname(shpth), "gad_rg_whole.shp"))

# TIMP
shpth = '~/src/glac-gee/shp/timp-2025/timp-narrow-2025.shp'
v = vect(shpth)
plot(v)
v2 = project(v, "epsg:4326")
# 40°23'13"N 111°38'23"W
pt1 = vect(cbind(-111.639722, 40.386944), crs=crs(v2))
pt2 = vect(cbind(-111.640556, 40.3875), crs=crs(v2))
plot(v2)
plot(pt1,add=T, cex=2)
plot(pt2,add=T, cex=2, col='red')
# And 
# 40°23'15"N 111°38'26"W
pt2 = c(40.3875, -111.640556)

writeVector(pt1, file.path(dirname(shpth), "pt1.shp"))
writeVector(pt2, file.path(dirname(shpth), "pt2.shp"))
