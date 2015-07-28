crimeGUI <- function(db = NULL) {    
    require(gWidgets)
    require(gWidgetsRGtk2)
    require(RGtk2)
    options("guiToolkit"="RGtk2")


    # Define variables
    google_map_types <- c("terrain", "roadmap", "satellite", "hybrid")
    crime_types      <- c("Homicide", "Assault", "Kidnapping", "Robbery & Arson", 
                          "Embezzlement and Extortion", "Stolen Vehicle", "Stolen or Damanged Property", "All Others")
    firstDB          <- ifelse(is.null(db), file.path(getwd(), dir(pattern=".db")[1]), db)
    startDate        <- "2012-01-01"
    endDate          <- "2012-01-31"

    # Define Handlers
    gdb  <- function(h, ...) {svalue(dbaddress) <- gfile()}  # User-interface to select database file
    map  <- function(h, ...) {
        source("gmap.r")

        on.exit(dispose(win), add = TRUE)
        if (!is.na(svalue(start_date)))
            startDate <- svalue(start_date)
        if (!is.na(svalue(end_date)))
            endDate <- svalue(end_date)
            
        param         <- list()
        param$db      <- svalue(dbaddress)
        param$start   <- startDate
        param$end     <- endDate
        param$maptype <- svalue(map_type)
        param$crimes  <- c(svalue(crime_type1, index = TRUE), 
                           svalue(crime_type2, index = TRUE) + 4)

        do.call("gmap", param)
    }  # end gmap
    exit <- function(h, ...) {dispose(win)}                  # Cancel button


    # Window has container group
    #   --Container group has Options group and Control group
    #       --Options group has Select Group and Type Group
    #           --Select group has DB Frame and Date Frame
    #             --Contains DB selector and date selectors
    #           --Type group has Map Frame and Crime Frame
    #             --Contains Map type radio and Crime type checkboxes
    #       --Control group has submit button only
    win           <- gwindow("Crime Mapper", visible = FALSE, height = 150)
    container     <- ggroup(horizontal = FALSE,          container = win)

    options_group <- ggroup(horizontal = FALSE,          container = container)
    control_group <- ggroup(horizontal = TRUE,           container = container)
    select_group  <- ggroup(horizontal = FALSE,          container = options_group)
    type_group    <- ggroup(horizontal = TRUE,           container = options_group)

    db_frame      <- gframe("Select Database",           container = select_group)
    date_frame    <- gframe("Select Crime Period",       container = select_group)
    map_frame     <- gframe("Map Type",                  container = type_group)
    crime_frame   <- gframe("Crime Type",                container = type_group)
    cancel_button <- gbutton("Cancel", handler = exit,   container = control_group)
    submit_button <- gbutton("SUBMIT", handler = map,    container = control_group)

    dbaddress     <- gedit(firstDB, 40,                  container = db_frame)
    findbutton    <- gbutton("DB File", handler = gdb,   container = db_frame)
    start_date    <- gcalendar(startDate, width = 10,    container = date_frame)
    end_date      <- gcalendar(endDate, width = 10,      container = date_frame)
    map_type      <- gradio(google_map_types,            container = map_frame)
    crime_type1   <- gcheckboxgroup(crime_types[1:4], T, container = crime_frame)
    crime_type2   <- gcheckboxgroup(crime_types[5:8], T, container = crime_frame)
    visible(win)  <- TRUE
}  # end function
