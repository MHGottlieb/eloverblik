  
  library(tools)
  library(ggplot2)
  library(ggridges)
  library(reshape2)
  
  data <- read_xlsx("Eloverblik.dk  - Måledata.xlsx")
  data <- unique(data)
  data$date <- as.POSIXct(data$Til, format="%d-%m-%Y %H:%M:%S")
  data <- data[data$date>1517439600,]
  
  data$kwh <- as.numeric(gsub(",", ".", data$Værdi))
  data$hour <- as.numeric(format(data$date, "%H"))
  data$day <- toTitleCase(weekdays(data$date))
  data$month <- factor(toTitleCase(months.POSIXt(data$date)))
  data$day <- ordered(data$day, levels=c("Mandag", "Tirsdag", "Onsdag", "Torsdag", 
                                       "Fredag", "Lørdag", "Søndag"))
  data$month <- factor(data$month, levels=c("Januar", "Februar", "Marts", 
                                            "April", "Maj", "Juni", "Juli", 
                                            "August", "September", "Oktober", 
                                            "November", "December"))
  data$monthmean <- ave(data$kwh, data$month, FUN=mean)

  data$Fra <- NULL
  data$Til <- NULL
  data$Værdi <- NULL
  data$Enhed <- NULL
  
  df <- aggregate(data$kwh, list(data$day, data$hour), mean)
  names(df) <- c("day", "hour", "kwh")
  df$daily <- ave(df$kwh, df$day, FUN=sum)

# Charts ------------------------------------------------------------------

  ggplot(df, aes(x=hour, y=day, group=day, height=kwh, fill=daily)) +
    geom_ridgeline(stat="identity", scale=8, alpha=0.6, size=0.6) +
    labs(title="Døgnrytmen hos Magnus", 
         subtitle="Elforbrug per time, 1. februar 2018 - 13. januar 2019", 
         caption="Data fra eloverblik.dk",
         fill="Gns. kWh\nper dag",
         x="Time", y=NULL) +
    theme(axis.title.y = element_blank(), axis.text.y=element_text(hjust=0)) +
    scale_fill_viridis(option="B")

  ggplot(data, aes(x=month, y=kwh, group=month, fill=monthmean*24)) + 
    geom_boxplot(outlier.shape=NA, size=0.4, notch = TRUE) +
    coord_flip() +
    scale_fill_viridis(option="B") +
    labs(title="Elforbrug højere om sommeren", 
       subtitle="Gns. elforbrug per måned, 1. februar 2018 - 13. januar 2019", 
       caption="Data fra eloverblik.dk",
       x="Måned", y="Gns. elforbrug (kW)",
       fill="Gns. kWh\nper dag") +
    ylim(0,0.25) +
    theme(axis.title.y = element_blank(), axis.text.y=element_text(hjust=0))
  
  ggplot(data, aes(x=hour, y=as.Date(date), fill=kwh))+
    geom_raster(interpolate=TRUE) +
    scale_fill_viridis(option="B", direction=1, begin=0.1, end=1) +
    labs(title="Året der gik", 
         subtitle="Elforbrug per time, 1. februar 2018 - 13. januar 2019", 
         caption="Data fra eloverblik.dk",
         x="Time",
         fill="kW") +
    scale_y_date(date_breaks = "1 month", date_labels =  "%B %Y") +
    theme(axis.title.y = element_blank(), axis.text.y=element_text(hjust=0))
  