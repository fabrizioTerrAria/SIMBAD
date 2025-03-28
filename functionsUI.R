

updateBoxKnobInput <- function(boxTitle,label,redPercInput,readOnly) {
  
  if (missing(redPercInput) & missing(readOnly)) {
    
    redPercInput <- matrix(0, nrow = length(PRECURSORS), ncol = length(SECTORS_ID))
    readOnly <- FALSE
    
  }
  
  box(title = boxTitle,
      width = 12,
      collapsible = T,
      lapply(1:(length(SECTORS)-1), FUN = function(j) {
        column(width = 12,
               align = 'center',
               h6(SECTORS_DESC[j]),
               hr(),
               fluidRow(
                 lapply(1:length(PRECURSORS), FUN = function(i) {
                   column(width = 12/length(PRECURSORS),
                          align = 'center',
                          div(id = "knobElement",
                              knobInput(
                                inputId = paste0(label,j,"-",i),
                                label = paste0(PRECURSORS_DESC[i]),
                                value = round(redPercInput[i,j],digits = 0),
                                min = -100,
                                max = 100,
                                post = "%",
                                angleArc = 270,
                                angleOffset = -135,
                                displayPrevious = FALSE,
                                lineCap = "default",
                                rotation = "clockwise",
                                bgColor = "#EEEEEE",
                                fgColor = "#87CEEB",
                                inputColor = "#83878a",
                                width = "100%",
                                height = "100%",
                                readOnly = readOnly
                              )
                          )
                   )
                 })
               )
        )
      })
  )
  
}




updatePolyMap <- function(mapName,pal,labels,shapefile,values,minMaxVal,title,classSize,classToHide) {
  
  leafletProxy(mapName, data = shapefile) %>%
    leaflet.extras2::addSpinner()  %>%
    leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 15)) %>%
    clearShapes() %>%
    clearControls() %>%
    clearImages() %>%
    clearPopups() %>%
    addPolygons(
      group = "dati",
      fillColor = ~pal(values),
      weight = 0.5,
      opacity = 1,
      color = "grey",
      dashArray = "3",
      fillOpacity = 1,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"),
      # layerId=~shpConc$NAME
    ) %>%
    addLegend(pal = pal, values = minMaxVal, opacity = 0.7, title = title, position = "bottomright")	%>%
    addEasyprint(options = easyprintOptions(
      # title = 'Print map',
      sizeModes = list("Custom Size"=list(
        width= 800,
        height= 800,
        name = "A custom landscape size tooltip",
        className = classSize)
      ),
      # position = 'topleft',
      hidden = TRUE,
      hideControlContainer = FALSE,
      hideClasses = classToHide,
      tileWait = 2000,
      exportOnly = TRUE)) %>%
    leaflet.extras2::stopSpinner() 
  
} 

updateRasterMap <- function(mapName,pal,rasterfile,values,minMaxVal,title,classSize,classToHide) {
  
  leafletProxy(mapName) %>%
    leaflet.extras2::addSpinner()  %>%
    leaflet.extras2::startSpinner(options = list("lines" = 12, "length" = 15)) %>%
    clearShapes() %>%
    clearControls() %>%
    clearImages() %>%
    clearPopups() %>%
    addRasterImage(rasterfile, colors = pal, opacity = 0.7) %>%
    addLegend(pal = pal, values = minMaxVal, opacity = 0.7, title = title, position = "bottomright")	%>%
    addEasyprint(options = easyprintOptions(
      # title = 'Print map',
      sizeModes = list("Custom Size"=list(
        width= 800,
        height= 800,
        name = "A custom landscape size tooltip",
        className = classSize)
      ),
      # position = 'topleft',
      hidden = TRUE,
      hideControlContainer = FALSE,
      hideClasses = classToHide,
      tileWait = 2000,
      exportOnly = TRUE)) %>%
    leaflet.extras2::stopSpinner() 
  
}


writeSHP <- function(shpExtraction,type,path) {
  
  if (type == "EMIX") {
    
    shpExtractionNew <- lapply(shpExtraction,function(x) {
      x %>%
        rename_with(., ~ sub("deltaP_", "dP_", .), starts_with("deltaP_")) %>%
        rename_with(., ~ sub("delta_", "d_", .), starts_with("delta_")) %>%
        rename(dP_PPCORHN = dP_PPCO_RHN) %>%
        rename(dP_PPCORHB = dP_PPCO_RHB) %>%
        rename(dP_PP25RHN = dP_PP25_RHN) %>%
        rename(dP_PP25RHB = dP_PP25_RHB)
    }) 
    
  } else {
    
    shpExtractionNew <- shpExtraction
    
  }
  
  st_write(cbind(shp[[1]],shpExtractionNew[[1]]), paste0(path,"/","comuni.shp"), append = F)
  st_write(cbind(shp[[2]],shpExtractionNew[[2]]), paste0(path,"/","province.shp"), append = F)
  st_write(cbind(shp[[3]],shpExtractionNew[[3]]), paste0(path,"/","regioni.shp"), append = F)
  
}



modalDialogFunction <- function(failed, text) {
  
  if (failed) {
    
    style <- "color: red;"
    title <- "Errore"
    
  } else  {
    
    style <- "color: green;"
    title = "Ok"
    
  }
  
  modalDialog(
    title = title,
    div(tags$b(text, style = style)),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Close"),
    )
  )
  
}


# messaggio per check file input
sendAlert <- function(text) {
  
  showModal(
    modalDialogFunction(TRUE, text)
  )
  
}

# messaggio per reset password
resetPswModal <- function(failed, username) {
  
  if (is.null(failed)) {
    
    modalDialog(
      title = "Confirm",
      # paste0("Are you sure, you want to reset password for user ", username, "?\nAn email will be sent to the user!"),
      span("Are you sure, you want to reset password for user ", tags$b(username), "?"),
      div(tags$i("An email will be sent to the user!", style = "color: grey;")),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("resetPswUser", "OK")
      )
    )
    
  } else if (failed) {
    
    modalDialog(
      title = "Error",
      div(tags$b("Error: mail not sent!", style = "color: red;")),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
      )
    )
    
  } else  {
    
    modalDialog(
      title = "Great",
      div(tags$b("Mail sent successfully!", style = "color: green;")),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
      )
    )
    
  }
}

# messaggio per nuovo utente
newUserModal <- function(failed) {
  
  if (is.null(failed)) {
    
    modalDialog(
      title = "Create new user",
      textInput(inputId = "UserName",label = "Username"),
      textInput(inputId = "FirstName",label = "First Name"),
      textInput(inputId = "LastName",label = "Last Name"),
      textInput(inputId = "Email",label = "Email"),
      selectInput(inputId = "Group",label = "Group",choices = c("ADMIN","USER")),
      div(tags$i("An email will be sent to the user!", style = "color: grey;")),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("saveNewUser", "Save")
      )
    )
    
  } else if (failed) {
    
    modalDialogFunction(failed, "Errore nell'invio della mail!")
    
  } else  {
    
    modalDialogFunction(failed, "Mail inviata correttamente!")
    
  }
}