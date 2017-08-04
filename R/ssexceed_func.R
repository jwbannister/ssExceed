
#' strip legend from ggplot object
#' 
#' @param a.gplot ggplot object.
#' @return A grob of the plot legend
g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  dev.off()
  return(legend)
} 

#' Plot DCA areas with background
#' 
#' @import dplyr
#' @import ggplot2
#' @param polys_df 
#' @param labels_df 
#' @param external_points 
plot_extended_background <- function(xmin, xmax, ymin, ymax){
    map <- raster::stack("~/gis/salton/landsat/landsat8_salton_120m.tif")
    ext <- sp::SpatialPointsDataFrame(coords=data.frame(x=c(xmin, xmax), 
                                                        y=c(ymin, ymax)), 
                                      data=data.frame(id=1:2), 
                                      proj4string=raster::crs(map))
    map_sub <- raster::crop(map, raster::extent(ext))
    map_sub <- raster::aggregate(map_sub, 4)
    map_df <- raster::as.data.frame(map_sub, xy=T)
    map_df <- data.frame(x=map_df$x, y=map_df$y, r=map_df[ , 3], g=map_df[ , 4], 
                         b=map_df[ , 5])
    map_df[is.na(map_df)] <- 0
    for (i in c('r', 'g', 'b')){
        map_df[[i]] <- 
        sapply(map_df[[i]], function(x) 
        (x - min(map_df[[i]])) * (255/(max(map_df[[i]]) - min(map_df[[i]]))))
    }
    p1 <- ggplot(data=map_df) + coord_fixed() + theme_bw() +
        geom_tile(aes(x=x, y=y, fill=rgb(r,g,b, maxColorValue = 255)), alpha=0.75) + 
        scale_fill_identity() + 
        scale_x_continuous(breaks=range(map_df$x)*c(1.01, 0.99), 
                           labels=range(map_df$x), expand = c(0,0)) +
        scale_y_continuous(breaks=range(map_df$y)*c(0.99, 1.01), 
                           labels=range(map_df$y), expand = c(0,0)) +
        theme(panel.grid=element_blank(), 
              axis.title=element_blank(), 
              axis.text=element_blank(), 
              axis.ticks=element_blank(), 
              plot.title=element_text(hjust=0.5))
    p1
}

event_plot <- function(locs, df1, background){
    a <- list(grobs=c(), centers=c())
    valueseq <- c(10, 50, 150, 500)
    legend.plot <- df1 %>% filter(deployment==df1$deployment[1]) %>%
        plot_rose(., value='pm10', dir='wd', valueseq=valueseq,
                  legend.title="PM10")
    legnd <- g_legend(legend.plot)
    fl <- tempfile()
    for (j in unique(df1$deployment)){
        p <- filter(df1, deployment==j) %>% 
            plot_rose_image_only(., value='pm10', dir='wd', valueseq=valueseq)
        png(filename=fl, bg="transparent")
        print(p)
        dev.off()
        img <- png::readPNG(fl)
        ras <- grid::rasterGrob(img, interpolate=TRUE)
        a$grobs[[j]] <- ras
        a$centers[[j]] <- c(filter(locs, deployment==j)$x, 
                            filter(locs, deployment==j)$y)
    }
    legend_bottom <- min(background$data$x)
    legend_top <- min(background$data$x) + 
        0.2*(max(background$data$x)-min(background$data$x))
    legend_left <- min(background$data$y)
    legend_right <- min(background$data$y) +
        0.25*(max(background$data$y)-min(background$data$y))
    p3 <- background + coord_cartesian() +  
        annotation_custom(legnd, xmin=legend_bottom, xmax=legend_top, 
                          ymin=legend_left, ymax=legend_right) +
        coord_fixed()
    for (i in 1:length(a$grobs)){
        p3 <- p3 + annotation_custom(a$grobs[[i]], xmin=a$centers[[i]][1]-15000,
                                     xmax=a$centers[[i]][1]+15000, 
                                     ymin=a$centers[[i]][2]-15000,
                                     ymax=a$centers[[i]][2]+15000) 
    }
    p3
}

report_header <- function(event_date, report_date){
    cat("<img style=\"float: right;\" src=\"../data/logo.png\"> \n")
    cat(" \n# ", format(event_date, "%m-%d-%Y"), " PM10 Exceedance \n")
    cat(" \n## IID Air Quality Program\n")
    cat(" \n##### Report Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}
