
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

event_plot <- function(locs, df1, label_df, background){
    valueseq <- c(50, 150, 250, 500)
    legend.plot <- df1 %>% filter(deployment==df1$deployment[1]) %>%
        plot_rose(., value='pm10', dir='wd', valueseq=valueseq,
                  legend.title=bquote('P'*M[10]~'('*mu*'g/'*m^3*')'), 
                  palette='RdYlGn')
    legnd <- g_legend(legend.plot)
    wind_hours <- df1 %>% group_by(deployment) %>%
        summarize(n_wd=sum(!is.na(wd)))
    a <- list(grobs=c(), centers=c(), labels=c())
    fl <- tempfile()
    for (j in unique(df1$deployment)){
        tp <- filter(df1, deployment==j, !is.na(wd)) 
        if (nrow(tp)>0 & wind_hours[wind_hours$deployment==j, ]$n_wd>=18){
            p <- plot_rose_image_only(tp, value='pm10', dir='wd', 
                                      valueseq=valueseq, palette='RdYlGn') 
            png(filename=fl, bg="transparent")
            print(p)
            dev.off()
            img <- png::readPNG(fl)
            ras <- grid::rasterGrob(img, interpolate=TRUE)
            a$grobs[[j]] <- ras
        } else{
            a$grobs[[j]] <- grid::circleGrob(r=0.04, gp=grid::gpar(fill='grey'))
        }
        a$centers[[j]] <- c(filter(locs, deployment==j)$x, 
                            filter(locs, deployment==j)$y)
    }
    legend_top <- max(background$data$y)
    legend_bottom <- max(background$data$y) - 
        0.3*(max(background$data$y)-min(background$data$y))
    legend_right <- max(background$data$x)
    legend_left <- max(background$data$x) -
        0.25*(max(background$data$x)-min(background$data$x))
    p3 <- background + coord_cartesian() +  
        annotation_custom(legnd, xmin=legend_left, xmax=legend_right, 
                          ymin=legend_bottom, ymax=legend_top) +
        coord_fixed()
    for (i in 1:length(a$grobs)){
            p3 <- p3 + annotation_custom(a$grobs[[i]], xmin=a$centers[[i]][1]-15000,
                                         xmax=a$centers[[i]][1]+15000, 
                                         ymin=a$centers[[i]][2]-15000,
                                         ymax=a$centers[[i]][2]+15000) 
    }
    for (m in 1:nrow(filter(label_df, flag))){
        tmp <- filter(label_df, flag)[m, ]
        if(nrow(tmp)>0){
        p3 <- p3 + 
            ggrepel::geom_label_repel(data=tmp, mapping=aes(x=x, y=y, label=label), 
                                      nudge_x=tmp$x_adj[1], nudge_y=tmp$y_adj[1],
                                      min.segment.length=unit(0, "lines"), 
                                      size=2, fontface='bold') 
        }
    }
    for (n in 1:nrow(filter(label_df, !flag))){
        tmp <- filter(label_df, !flag)[n, ]
        if (nrow(tmp)>0){
        p3 <- p3 + 
            ggrepel::geom_label_repel(data=tmp, mapping=aes(x=x, y=y, label=label), 
                                      nudge_x=tmp$x_adj[1], nudge_y=tmp$y_adj[1],
                                      min.segment.length=unit(0, "lines"), 
                                      size=2) 
    }
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

summary_header <- function(){
    cat("<img style=\"float: right;\" src=\"../data/logo.png\"> \n")
    cat(" \n# PM10 Exceedances ", format(start_date, "%m-%d-%Y"), " through ", 
        format(end_date, "%m-%d-%Y"), " \n")
    cat(" \n## IID Air Quality Program\n")
    cat(" \n##### Report Date: ", report_date, " \n") 
    cat("<hr class=\"style1\">")
}

adjust_labels <- function(label_df){
    adj_df <- data.frame(deployment=c('PalmFire', 'IndioJack', 'TM Admin Station', 
                                      'Torres Martinez', 'Bombay Beach', 
                                      'Salton City', 'Niland', 'Naval Test Base', 
                                      'Sonny Bono', 'Westmor-1st', 'EC-9', 
                                      'Calexico', 'Salton Sea Park', 'Brawley', 
                                      'Calexico-E'), 
                         x_adj=c(-10000, 10000, -20000, -20000, 20000, -15000, 
                                 15000, -20000, -10000, -15000, -10000, -20000, 
                                 20000, 10000, 10000), 
                         y_adj=c(-6000, 6000, 0, -6000, 0, 0, 0, 0, -10000, 
                                 -10000, 0, 0, 6000, 0, 0))
    label_df <- left_join(label_df, adj_df, by="deployment")
    label_df$x_adj <- sapply(label_df$x_adj, function(x) ifelse(is.na(x), 0, x))
    label_df$y_adj <- sapply(label_df$y_adj, function(x) ifelse(is.na(x), 0, x))
    label_df
}
