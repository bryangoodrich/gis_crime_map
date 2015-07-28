cacheExtracts <- function(cached = TRUE) {
  # ===== Define required libraries =====
  require(RCurl)  # For requesting URLs from sacpd.org
  require(chron)  # For date conversion into ordered factor components (y,m,d)
  require(rgdal)  # For coordinate conversion from State Plane to geographic



  # ===== Define Helper Functions =====
  # Reads in a table file contained in a zip file
  readZIP <- function(zipfile, infile, cached, ...) {
    tpath <- file.path("zipcache", 
                       paste(substr(infile, 1, nchar(infile)-3), "zip", sep = ""))
    if (!cached) {                   
      if (!file.create(tpath))
        stop("File could not be created")
      bin <- getBinaryURL(zipfile)
      zipcon <- file(tpath, open = "wb")
      writeBin(bin, zipcon)
      close(zipcon)
    }  # end if
    
    data <- read.table(unz(tpath, infile), ...)
    
    return(data)
  }  # end function

  # Converts the date and time string to a date only string
  makeDate <- function(date) {
    dateClean <- sapply(strsplit(as.character(date), ' '), function(x) x[1])
    dateClean <- as.character(as.Date(dateClean, format = "%m/%d/%Y"))
    return(dateClean)
  }  # end function

  # Takes the URL to the reports page containing links to the ZIP files
  # and returns a vector of the full URL paths to each one
  findZip <- function(url) {
    urlRequest <- getURL(paste(url, "reports/", sep = ""))
    g <- gregexpr("data/.*?zip'", urlRequest)
    g <- paste(url,
               "data/", 
               substring(urlRequest,
                         g[[1]]+5,
                         g[[1]] + attributes(g[[1]])$match.length-2),
               sep = "")
    return(g)
  }  # end function



  # ===== Define Required Variables =====
  # SRID: 2226
  # PROJ.4 string to invert the California State Plane II (feet) projection
  # back into lon/lat values to be used for Google Map production later
  projStr <- paste(
    "+proj=lcc +lat_1=38.33333333333334 +lat_2=39.83333333333334 ",
    "+lat_0=37.66666666666666 +lon_0=-122 +x_0=2000000 ",
    "+y_0=500000.0000000002 +ellps=GRS80 +datum=NAD83 ",
    "+to_meter=0.3048006096012192 +no_defs", sep = "")
  

  
  # ===== Obtain the ZIP file locations =====
  cat("===== Begin Caching Routine =====\n\n")
  cat("Obtaining ZIP file locations\n")
  zipfiles <- findZip("http://www.sacpd.org/crime/stats/")



  # ===== Download ZIP Files =====
  cat("Beginning ZIP file downloads\n\n")
  if (!cached) {
    tdir <- file.path(getwd(), "zipcache")
    if (file.exists(tdir))  # if 'zipcache' directory exists, delete it and its contents 
      unlink(tdir, TRUE)
      
    if(!dir.create(tdir, showWarnings = FALSE))
      stop("Caching directory could not be created")
  }  # end if
  
  crime <- lapply(seq_along(zipfiles), function(n) {
      infile <- paste(substr(zipfiles[n], 39, nchar(zipfiles[n])-3),
                      "csv", sep = "")
      df <- readZIP(zipfiles[n], infile, cached, header = TRUE, sep = ",")
      return(df)
    }
  )
  cat("Downloads Finished\n")


  # ===== Transform Data and Convert Projected Coordinates =====
  cat("Begin Data Transformation\n")

  crime <- do.call("rbind", crime)
  crime <- unique(crime)  # Remove duplicated rows
  crime <- crime[crime$X_Coord != 0, ]
  cat("Turned into Data Frame\n")

  crime <- transform(crime,
                     OccDate = makeDate(OccDate),
					           Code    = as.integer(substr(Code, 1, 4)))
  cat("Time elements added\n")
  
  
  
  # Recode UOC 
  breaks <- c(0, 900, 1000, 1100, 1200, 1300, 1400, 2000, 
              2100, 2200, 2400, 2500, 2700, 2800, 3000, 9000)
  bins   <- c(8, 1, 3, 8, 4, 2, 8, 4, 5, 8, 6, 8, 5, 7, 8)
  crime <- transform(crime, 
                 SimpleCode = cut(Code, breaks, include.lowest = TRUE))
  levels(crime$SimpleCode) <- bins
    
    
  # Change projected coordinates
  xy <- data.frame(project(as.matrix(crime[8:9]), projStr, inv = TRUE))
  names(xy) <- c('lon', 'lat')
  cat("Coordinate conversion table completed\n")

  crime <- cbind.data.frame(crime, xy)
  cat("Final table generated\n")
  

  # Create output file timestamp
  filename <- paste("crime", 
                    format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
                    "rda", sep = ".")
  
  cat("Saving data table 'crime' to", filename, "\n")
  save(crime, file = filename, compress = "xz")

  cat("===== Crime data caching completed =====\n")
  
  return(filename)
}  # end function

