library(tidyverse)

# pull IID data
query_teom <- paste0("SELECT i.deployment, t.datetime, t.pm10_stp AS pm10, 
                     t.pm25, ", 
                     "flags.field_is_invalid(t.deployment_id, 145, t.datetime) ",
                     "AS invalid_pm10, ",
                     "flags.field_is_invalid(t.deployment_id, 146, t.datetime) ",
                     "AS invalid_pm25, ",
                     "m.ws_10m AS ws, m.wd_10m AS wd, ",
                     "flags.field_is_invalid(t.deployment_id, 236, t.datetime) ",
                     "AS invalid_ws, ",
                     "flags.field_is_invalid(t.deployment_id, 298, t.datetime) ",
                     "AS invalid_wd ",
                     "FROM teom.pm_1hour t LEFT JOIN info.deployments i ",
                     "ON t.deployment_id = i.deployment_id ", 
                     "LEFT JOIN met.met_1hour m ",
                     "ON t.deployment_id=m.deployment_id ",
                     "AND t.datetime=m.datetime ",
                     "WHERE (t.datetime - '1 second'::interval)::date ",
                     "BETWEEN '", start_date, "'::date ",
                     "AND '", end_date, "'::date;")
pm_pull <- query_db("saltonsea", query_teom)
pm_df <- pm_pull %>% arrange(deployment, datetime) %>% filter(!invalid_pm10) %>%
    filter(!invalid_pm25) %>%
    select(-invalid_pm10, -invalid_pm25, -invalid_wd, -invalid_ws)

# pull AQMIS data
query_aqmis <- paste0("SELECT i.deployment, t.datetime, t.pm10, ", 
                     "flags.field_is_invalid(t.deployment_id, 162, t.datetime) ",
                     "AS invalid_pm10, m.ws, m.wd, ",
                     "flags.field_is_invalid(t.deployment_id, 165, t.datetime) ",
                     "AS invalid_ws, ",
                     "flags.field_is_invalid(t.deployment_id, 166, t.datetime) ",
                     "AS invalid_wd ",
                     "FROM aqmis.pm_1hour t LEFT JOIN info.deployments i ",
                     "ON t.deployment_id = i.deployment_id ", 
                     "LEFT JOIN aqmis.met_1hour m ",
                     "ON t.deployment_id=m.deployment_id ",
                     "AND t.datetime=m.datetime ",
                     "WHERE (t.datetime - '1 second'::interval)::date ",
                     "BETWEEN '", start_date, "'::date ",
                     "AND '", end_date, "'::date;")
aqmis_pull <- query_db("saltonsea", query_aqmis)
if (nrow(aqmis_pull)>0){
aqmis_df <- aqmis_pull %>% arrange(deployment, datetime) %>% 
    filter(!invalid_pm10) %>% mutate(pm25=rep(NA, length(deployment))) %>%
    select(-invalid_pm10, -invalid_wd, -invalid_ws)
    df1 <- rbind(pm_df, aqmis_df)
} else{
    df1 <- pm_df
}


# pull deployment locations
query1 <- paste0("SELECT deployment, ",
                 "ST_X(ST_TRANSFORM(geom, 26911)) AS x, ",
                 "ST_Y(ST_TRANSFORM(geom, 26911)) AS y ",
                 "FROM info.deployments ", 
                 "WHERE deployment IN ('", 
                 paste(c(unique(df1$deployment), 'PalmFire'), collapse="', '"), 
                 "');")
loc_df <- query_db("saltonsea", query1)
loc_df <- filter(loc_df, x>=loc_df$x[loc_df$deployment=='PalmFire'] & 
                 y<=loc_df$y[loc_df$deployment=='PalmFire'])

hour_df <- filter(df1, deployment %in% loc_df$deployment)

# pull photo data
query2 <- paste0("SELECT i.datetime, d.deployment, i.image_deployment_id, f.s3_url ", 
                 "FROM images.images i ", 
                 "JOIN images.image_files f ON i.image_file_id=f.image_file_id ", 
                 "JOIN images.image_deployments id ", 
                 "ON f.image_deployment_id = id.image_deployment_id ", 
                 "JOIN info.deployments d ON id.deployment_id=d.deployment_id ",
                 "WHERE d.deployment != '1004' ", 
                 "AND i.image_deployment_id != '3' ",
                 "AND (datetime - '1 second'::interval)::date ",
                 "BETWEEN '", start_date, "'::date ",
                 "AND '", end_date, "'::date;")
image_df <- query_db("saltonsea", query2)
