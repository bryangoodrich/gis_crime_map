gmap <- function(db, start, end, maptype, crimes) {
  require(RSQLite)
  require(RgoogleMaps)
  require(RColorBrewer)

  # Connect to database and subset based on provided parameters
  drv <- dbDriver("SQLite")
  dbcon <- dbConnect(drv, db)
  on.exit(dbDisconnect(dbcon), add = TRUE)
  fmt   <- paste("SELECT * FROM crime as c",
                 "INNER JOIN crime_type as ct",
                 "ON c.SimpleCode = ct.SimpleCode",
                 "WHERE c.SimpleCode IN (%s)",
                 "AND  date(c.OccDate) BETWEEN date('%s') AND date('%s')",
                 "ORDER BY c.OccDate")
  query <- sprintf(fmt, paste(crimes, collapse=","), start, end)
  res <- dbSendQuery(dbcon, query)
  df <- fetch(res, -1)
  dbClearResult(res)

   
  #Set up plotting structures
  pal <- brewer.pal(8, "Set1")[crimes]  # Each crime has its own color
  sym <- 19                              # pch symbol to use, can take multiple
  
  
  # Plot Map
  bb    <- qbbox(df$lat, df$lon)
  mymap <- GetMap.bbox(bb$lonR, bb$latR, maptype = maptype)
  #mymap <- GetMap.bbox(center = c(38.515, -121.4725), zoom = 11, maptype = maptype)  
  mymap <- PlotOnStaticMap(mymap, lat = df$lat, lon = df$lon, verbose = 0,
                           cex=1, pch=sym, col=pal)
  return(TRUE)


}  # end function


