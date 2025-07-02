
rgpth = "/Users/mattolson/src/glac-gee/results"

fls = list.files(rgpth, pattern = "narrow", full.names = T)
tables = lapply(fls, function(x){ddf = read.csv(x);ddf$sensor = strsplit(basename(x), "_")[[1]][1];return(ddf)})
combined_df <- do.call(rbind, tables)

combined_df %>% 
  ggplot(aes(x=year, y = area_m2_min, colour=sensor)) +
  geom_line()

combined_df %>% select(-c("date_closest", "area_m2_closest")) %>% 
  write.csv(., file.path(rgpth, "ndsi_1985_2024_timp_full.csv"), row.names = F)



fls = list.files(rgpth, pattern = "root", full.names = T)
tables = lapply(fls, function(x){ddf = read.csv(x);ddf$sensor = strsplit(basename(x), "_")[[1]][1];return(ddf)})
combined_df_root <- do.call(rbind, tables)

combined_df_root %>% 
  ggplot(aes(x=year, y = area_m2_min, colour=sensor)) +
  geom_line()


combined_df_root %>% select(-c("date_closest", "area_m2_closest")) %>% 
  write.csv(., file.path(rgpth, "ndsi_1985_2024_timp_root.csv"), row.names = F)
