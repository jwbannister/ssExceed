library(ggplot2)
library(tidyverse)
library(lubridate)
load_all("~/code/aiRsci")
load_all()

hour_df$date <- as.Date(hour_df$datetime %m-% seconds(1), tz="America/Los_Angeles")
pm10_cutoff <- 150
exceeds <- hour_df %>% filter(between(date, start_date, end_date)) %>%
    group_by(deployment, date) %>%
    summarize(pm10_24=sum(pm10)/length(pm10), pm25_24=sum(pm25)/length(pm25)) %>%
    filter(pm10_24>pm10_cutoff) %>% ungroup() %>% arrange(date)
exceed_days <- unique(exceeds$date)
event_summary <- hour_df %>% filter(date %in% exceed_days) %>%
    group_by(deployment, date) %>%
    summarize(pm10_24=sum(pm10)/length(pm10), pm25_24=sum(pm25)/length(pm25)) %>%
    ungroup() %>% arrange(date)
summary_table <- exceeds %>% arrange(date) %>%
    group_by(format(date, "%m-%d-%Y")) %>%
    summarize(n_stations=length(deployment), 
              max_pm10=round(max(pm10_24), 0))
names(summary_table) <- 
    c("Date of Exceedance", "# of Stations in Exceedance", 
      "Maximum Observed 24-hour PM<sub>10</sub>\n(ug/m<sup>3</sup>)")

# get background for use in dust rose plot
buffer <- 24000
background <- suppressWarnings(photo_background(min(loc_df$x)-buffer, 
                                               max(loc_df$x)+buffer, 
                                               min(loc_df$y)-buffer, 
                                               max(loc_df$y)+buffer, zone="11N"))
# set coordinates for determining daylight hours
salton_sea <- matrix(c(-115.8434, 33.3286), nrow=1) 

event_list <- vector(mode="list", length=length(exceed_days))
names(event_list) <- exceed_days
for (i in names(event_list)){ 
    # build table to display for each event day
    event_list[[i]]$table <- event_summary %>% filter(date==i) %>% 
        left_join(loc_df, by="deployment") %>% arrange(desc(y)) %>% 
        select(-x, -y, -date)
    exceed_stations <- filter(event_list[[i]]$table, pm10_24>150)$deployment
    event_list[[i]]$table$pm10_24 <- round(event_list[[i]]$table$pm10_24, 0)
    event_list[[i]]$table$pm25_24 <- round(event_list[[i]]$table$pm25_24, 0)
    event_list[[i]]$table$pm10_24 <- 
        sapply(event_list[[i]]$table$pm10_24, 
               function(x) ifelse(x>150, paste0("<b>",x, "</b>"), x))
    event_list[[i]]$table$pm10_24 <- sapply(event_list[[i]]$table$pm10_24, 
                                            function(x) ifelse(is.na(x), "-", x))
    event_list[[i]]$table$pm25_24 <- sapply(event_list[[i]]$table$pm25_24, 
                                            function(x) ifelse(is.na(x), "-", x))
    names(event_list[[i]]$table) <- c("Deployment",  
                       "24-hour PM<sub>10</sub> Avg. (ug/m<sup>3</sup>)", 
                       "24-hour PM<sub>2.5</sub> Avg. (ug/m<sup>3</sup>)") 
    # build timeseries plot
    event_df <- filter(hour_df, date==i)
    a <- select(event_df, deployment, datetime, value=pm10) %>% 
        mutate(factor="PM10 (ug/m^3)")
    b <- select(event_df, deployment, datetime, value=ws) %>% 
        mutate(factor="Wind Speed (m/s)")
    plot_df <- rbind(a, b)
    n2s <- loc_df %>% 
        filter(deployment %in% unique(plot_df$deployment)) %>%
        arrange(desc(y))
    plot_df$deployment <- factor(plot_df$deployment, levels=n2s$deployment, 
                                 ordered=T)
    plot_df$flag <- sapply(plot_df$deployment, 
                           function(x) ifelse(x %in% exceed_stations, T, F))
    timeseries <- plot_df %>%
        arrange(deployment, datetime) %>% 
        ggplot(aes(x=datetime, y=value)) +
        geom_path(aes(color=deployment, size=flag)) +
        scale_size_manual(name="", values=c(.5, .75), guide="none") +
        facet_grid(factor ~ ., scales="free_y") +
        ylab("") + xlab("") +
        theme(legend.title=element_blank(), 
              panel.grid.minor=element_blank(), 
              strip.text=element_text(size=4))
    event_list[[i]]$time_img <- paste0(tempfile(), ".png")
    png(filename=event_list[[i]]$time_img, width=8, height=3.5, units="in", 
        res=300)
    suppressWarnings(print(timeseries))
    dev.off()
    # build event photos
    event_list[[i]]$photos <- vector(mode="list", length=3)
    names(event_list[[i]]$photos) <- c("Torres Martinez", "1003", "Salton City")
    dt <- as.POSIXct(i, tz="America/Los_Angeles")
    sunrise <- maptools::sunriset(salton_sea, dt, direction="sunrise", 
                                  POSIXct.out=T)$time
    sunset <- maptools::sunriset(salton_sea, dt, direction="sunset", 
                                 POSIXct.out=T)$time
    daylight_df <- filter(event_df, between(datetime, sunrise, sunset)) %>%
        filter(date(datetime)==i)
    daylight_df$deployment <- sapply(daylight_df$deployment, 
                                     function(x) ifelse(x=='Sonny Bono', 
                                                         '1003', x))
    for (j in names(event_list[[i]]$photos)){
        tmp <- filter(daylight_df, deployment==j)
        if (nrow(tmp)==0){
            image.key <- c()
        } else{
            target.datetime <- tmp[tmp$pm10==max(tmp$pm10), ]$datetime
            image_tmp <- image_df %>% 
                filter(date(datetime)==date(target.datetime) & deployment==j) %>%
                mutate(delta = abs(difftime(datetime, target.datetime)))
            pic.datetime <- filter(image_tmp, delta==min(delta))$datetime
            image.key <- substring(filter(image_tmp, delta==min(delta))$s3_url, 49)
        }
        if (length(image.key)!=0){
            image.file <- tempfile()
            S3_bucket_access(bucket="saltonimages", key=image.key, 
                             file=image.file)
            img <- jpeg::readJPEG(image.file)
            prelim.grob <- grid::rasterGrob(img, interpolate=T)
            p1 <- ggplot(data.frame(x=1:10, y=1:10), aes(x=x, y=y)) +
                  theme(panel.background=element_blank(), 
                        axis.title=element_blank(), 
                        axis.text=element_blank(), 
                        axis.ticks=element_blank(),
                        legend.position="none", 
                        plot.title = element_text(hjust=0.5)) +
                  annotation_custom(prelim.grob, xmin=-Inf, xmax=Inf, 
                                    ymin=-Inf, ymax=Inf) +
                  ggtitle(j)
            image.grob <- ggplotGrob(p1)
        } else{
            p1 <- ggplot(data.frame(x=1:10, y=1:10), aes(x=x, y=y)) +
                  geom_blank() +
                  geom_text(aes(x=5, y=5, label="No Image Available", 
                                hjust="center")) + 
                  ggtitle(j) + 
                  theme(panel.background=element_blank(), 
                        axis.title=element_blank(), 
                        axis.text=element_blank(), 
                        axis.ticks=element_blank(), 
                        legend.position="none", 
                        plot.title = element_text(hjust=0.5)) 
            image.grob <- ggplotGrob(p1)
        }
        event_list[[i]]$photos[[j]] <- image.grob
    }
    event_list[[i]]$photo_img <- paste0(tempfile(), ".png")
    png(filename=event_list[[i]]$photo_img, width=8, height=1.9, units="in", 
        res=300)
        gridExtra::grid.arrange(grobs=list(event_list[[i]]$photos[['Torres Martinez']], 
                               event_list[[i]]$photos[['1003']], 
                               event_list[[i]]$photos[['Salton City']]), ncol=3)
    dev.off()
    # build dustrose map
    rose_data <- event_df %>% select(deployment, datetime, pm10, wd)
    wd_missing <- rose_data[is.na(rose_data$wd), 1:2]
    if (nrow(wd_missing)>0){
        wind_fill_query <- paste0("SELECT i.deployment, m.datetime, ",
                                  "COALESCE(m.wd_6m, m.wdv_2d) as WD ",
                                  "FROM met.met_1hour m JOIN info.deployments i ",
                                  "ON m.deployment_id=i.deployment_id ",
                                  "WHERE i.deployment IN ('",
                                  paste(unique(wd_missing$deployment), 
                                        collapse="', '"), "') ", 
                                  "AND (m.datetime - '1 second'::interval)::date=", 
                                  "'", i, "'::date;")
        wd_fill <- query_db("saltonsea", wind_fill_query)
        if (nrow(wd_fill)>0){
            rose_data <- rose_data %>% 
                left_join(wd_fill, by=c("deployment", "datetime")) %>%
                mutate(wd=coalesce(wd.x, wd.y)) %>% select(-wd.x, -wd.y)
        }
    }
    label_data <- event_summary %>% filter(date==i) %>%
        mutate(label=paste0(deployment, "\n", round(pm10_24, 0))) %>%
        left_join(loc_df, by="deployment")
    label_data$flag <- sapply(label_data$pm10_24, 
                              function(x) ifelse(x>150, T, F))
    label_data <- adjust_labels(label_data)
    event_list[[i]]$map <- event_plot(loc_df, rose_data, label_data, background)
    event_list[[i]]$map_img <- paste0(tempfile(), ".png")
    png(filename=event_list[[i]]$map_img, width=8, height=8, units="in", 
        res=300)
    print(event_list[[i]]$map)
    dev.off()
}
