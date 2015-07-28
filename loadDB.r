loadDB <- function(filename, db = "crime.db") {
  # ===== Load Required Libraries and Data =====
  require(RSQLite)
  load(filename)

  # Create crime type data set for later mapping
  crime_type <- data.frame(
    SimpleCode  = 1:8,
    Description = c("Homicide", "Assault", "Kidnapping", "Robbery & Arson", 
                    "Embezzlement & Extortion", "Stolen Vehicle", 
                    "Stolen or Damanged Property", "Others")
  );  #end data.frame
  
    
  # Delete any prior instances of database db
  if (file.exists(db)) {
    print ("Deleting prior copy of database")
    unlink(db)
  }  # end if

  # Initialize SQLite driver and database
  drv <- dbDriver("SQLite")
  con <- dbConnect(drv, db)
  on.exit(dbDisconnect(con), add = TRUE)
  cat("Database", db, "created\n")
    

  # Create the table structures for the crime and crime type data
  res <- dbSendQuery(con, "CREATE TABLE crime (
    row_names    INTEGER PRIMARY KEY ASC
    ,InternalID  TEXT
    ,OccDate     TEXT
    ,Location    TEXT
    ,Apartment   TEXT
    ,District    TEXT
    ,Beat        TEXT
    ,Grid        TEXT
    ,X_Coord     INT
    ,Y_Coord     INT
    ,Code        INT
    ,Description TEXT
    ,SimpleCode  INT
    ,lon         NUM
    ,lat         NUM)"
  )  # end create table
  dbClearResult(res)
  cat("Table crime created\n")
  
  res <- dbSendQuery(con, "CREATE TABLE crime_type (
    SimpleCode   INTEGER PRIMARY KEY ASC
    ,Description TEXT)"
  )  # end create table
  dbClearResult(res)
  cat("Table crime_type created\n")
  
  
  # Write the contents of the crime and crime type data to their tables
  dbWriteTable(con, "crime", crime, append = TRUE)
  cat("Contents of crime data loaded into database", db, "\n")
  
  dbWriteTable(con, "crime_type", crime_type, append = TRUE, row.names = FALSE)
  cat("Contents of Crime Type data loaded into database", db, "\n")

  
  # Define Indexes on date and code for later querying
  res <- dbSendQuery(con, "CREATE INDEX index_OccDate ON crime (OccDate)")
  dbClearResult(res)
  res <- dbSendQuery(con, "CREATE INDEX index_code    ON crime (SimpleCode)")
  dbClearResult(res)
  cat("Indexes Created\n")
  
  cat("Finished loading database\n")
  return(db)
}  # end function