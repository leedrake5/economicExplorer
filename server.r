library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)

function(input, output, session) {
    
    
    # Leaflet bindings are a bit slow; for now we'll just sample to compensate
    set.seed(100)
    
    
    zipdataInput <- reactive({
    if(input$fullmodel == FALSE) {
        hold <- allzips[sample.int(nrow(allzips), 10000),]
    } else if(input$fullmodel == TRUE) {
        hold <- allzips
    }
    
    hold
    
    })
    
    
    # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
    # will be drawn last and thus be easier to see
    zipdataToo <- reactive(
    {zipdata <- zipdataInput()
    zipdata[order(zipdata$centile),]
    })
    


  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
      zipdata <- zipdataToo()
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$centile,
      breaks = centileBreaks,
      main = "Political score (visible zips)",
      xlab = "Percentile",
      xlim = range(allzips$centile),
      col = '#00DD00',
      border = 'white')
  })

  output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)
      
      colorBy <- input$color
      sizeBy <- input$size
      
      cor.test <- lm(as.numeric(as.vector(zipsInBounds()[[colorBy]])) ~ as.numeric(as.vector(zipsInBounds()[[sizeBy]])))
      
      r2 <- summary(cor.test)$r.squared
      intercept <- cor.test$coef[1]
      slope <- cor.test$coef[2]
      

#print(xyplot(zipsInBounds()[[colorBy]] ~ zipsInBounds()[[sizeBy]], xlab=sizeBy, ylab=colorBy, type=c("p", "r"), main=expression(paste("y ="*paste(slope)*"x + "*paste(intercept)*", r"^2*paste(r2)))))

    temp.frame <- data.frame(as.numeric(as.vector(zipsInBounds()[[sizeBy]])), as.numeric(as.vector(zipsInBounds()[[colorBy]])))
    colnames(temp.frame) <- c("x", "y")
    temp.frame <- na.omit(temp.frame)
    
    scatter <- ggplot(aes(x, y), data=temp.frame) +
    geom_point(colour="blue") +
    stat_smooth(method="lm") +
    theme_light() +
    scale_x_continuous(sizeBy) +
    scale_y_continuous(colorBy) +
     annotate("text", label=lm_eqn(cor.test), x=0, y=Inf, hjust=0, vjust=1, parse=TRUE) 
    
    scatter
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
     zipdata <- zipdataToo()
    colorBy <- input$color
    sizeBy <- input$size

    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("Spectral", colorData)
    } else {
      colorData <- zipdata[[colorBy]]
      pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)
    }

    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    }

    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Score:", as.integer(selectedZip$centile)),
      tags$strong(HTML(sprintf("%s, %s %s",
        selectedZip$city.x, selectedZip$state, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
      sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
      sprintf("Adult population: %s", selectedZip$adultpop), tags$br(),
      tags$strong(HTML(sprintf("%s %s %s, %s",
      selectedZip$firstname, selectedZip$middlename, selectedZip$lastname, selectedZip$party
      ))), tags$br(),
      sprintf("Phone: %s", selectedZip$phone), tags$br(),
      sprintf("email: %s", selectedZip$oc_email), tags$br(),
      sprintf("Twitter: @%s", selectedZip$twitter_id)

      )
    )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  ####When zipcode is selected, show popup with city info
  
  observe({
      leafletProxy("map") %>% clearPopups()
      event <- as.numeric(paste(input$yourzipcode))
      zipframe <- subset(zipcodes, zipcodes$zip_code==event)
      
      
      
      if (is.null(event))
      return()
      
      isolate({
          showZipcodePopup(event, zipframe$latitude, zipframe$longitude)
      })
  })



  ## Data Explorer ###########################################
  
  reactiveZip <- reactive({
      
      smalls <- zipsInBounds()
      
      smalls$latitude <- jitter(smalls$latitude)
      smalls$longitude <- jitter(smalls$longitude)
      smalls$college <- smalls$college * 100
      smalls$unemployment <- smalls$Unemp..Rate * 100
      smalls$pubcov <- smalls$Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone
      smalls$medicare <- smalls$Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone
      smalls$va <- smalls$Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone
      smalls$medicaidexpansion <- smalls$Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold
      smalls$zipcode <- smalls$zipcode
      
      
      zoomtable <- smalls %>%
      select(
      City = city.x,
      State = state,
      Zipcode = zipcode,
      Rank = rank,
      Score = centile,
      Superzip = superzip,
      Population = adultpop,
      College = college,
      unemployment = Unemp..Rate,
      Income = income,
      Lat = latitude,
      Long = longitude
      )
      
      zoomtable
      
  })


  observe({
      
      
      
    cities <- if (is.null(input$states)) character(0) else {
      filter(reactiveZip(), State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
      selected = stillSelected)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      reactiveZip() %>%
        filter(State %in% input$states,
          is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
      selected = stillSelected)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
    
    
  })

  output$ziptable <- DT::renderDataTable({
      
      
    df <- reactiveZip() %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)

    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
  
  
  congressTable <- reactive({
      
      superzipInBounds <- zipsInBounds()
      superzipInBounds.dt <- data.table(superzipInBounds)
      
      
      
      superZipInBoundsCollapse <- superzipInBounds.dt[,
      list(zipcode=rank[1],
      number=rank[1],
      X=rank[1],
      centile=mean(na.rm=TRUE, as.numeric(as.vector(centile))),
      superzip=mean(na.rm=TRUE, as.numeric(as.vector(superzip))),
      rank=mean(na.rm=TRUE, as.numeric(as.vector(rank))),
      city=rank[1],
      adultpop=sum(na.rm=TRUE, as.numeric(as.vector(adultpop))),
      households=sum(na.rm=TRUE, as.numeric(as.vector(households))),
      college=mean(na.rm=TRUE, as.numeric(as.vector(college))),
      income=mean(na.rm=TRUE, as.numeric(as.vector(income))),
      State=rank[1],
      state=rank[1],
      Congressional.District=rank[1],
      Unemp..Rate=mean(na.rm=TRUE, as.numeric(as.vector(Unemp..Rate))),
      X..in.sample=sum(na.rm=TRUE, as.numeric(as.vector(X..in.sample))),
      Total..Estimate..Civilian.noninstitutionalized.population=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Civilian.noninstitutionalized.population))),
      Total..Margin.of.Error..Civilian.noninstitutionalized.population=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Civilian.noninstitutionalized.population))),
      Public.Coverage..Estimate..Civilian.noninstitutionalized.population=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Civilian.noninstitutionalized.population))),
      Public.Coverage..Margin.of.Error..Civilian.noninstitutionalized.population=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Civilian.noninstitutionalized.population))),
      Percent.Public.Coverage..Estimate..Civilian.noninstitutionalized.population=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Civilian.noninstitutionalized.population))),
      Percent.Public.Coverage..Margin.of.Error..Civilian.noninstitutionalized.population=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Civilian.noninstitutionalized.population))),
      Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone))),
      Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone))),
      Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone=sum(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone))),
      Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone=sum(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone))),
      Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))),
      Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))),
      Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))),
      Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicare.coverage.alone))),
      Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone))),
      Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone))),
      Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone))),
      Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...Medicaid.means.tested.coverage.alone))),
      Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))),
      Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))),
      Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))),
      Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..COVERAGE.ALONE...Public.health.insurance.alone...VA.health.care.coverage.alone))),
      Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
      Total..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
      Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
      Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
      Percent.Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
      Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))),
      Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
      Total..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
      Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
      Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
      Percent.Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
      Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...At.or.above.138.percent.of.the.poverty.threshold))),
      Total..Estimate..Worked.full.time..year.round..18.years.and.over.=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Worked.full.time..year.round..18.years.and.over.))),
      Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.))),
      Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over.=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over.))),
      Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.))),
      Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over.=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over.))),
      Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over.))),
      Total..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
      Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
      Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
      Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
      Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
      Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....18.to.64.years))),
      Total..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
      Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
      Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
      Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
      Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
      Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Worked.full.time..year.round..18.years.and.over....65.years.and.over))),
      Total..Estimate..Under.6=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..Under.6))),
      Total..Margin.of.Error..Under.6=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..Under.6))),
      Public.Coverage..Estimate..Under.6=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..Under.6))),
      Public.Coverage..Margin.of.Error..Under.6=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..Under.6))),
      Percent.Public.Coverage..Estimate..Under.6=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..Under.6))),
      Percent.Public.Coverage..Margin.of.Error..Under.6=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..Under.6))),
      Total..Estimate..6.to.17.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..6.to.17.years))),
      Total..Margin.of.Error..6.to.17.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..6.to.17.years))),
      Public.Coverage..Estimate..6.to.17.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..6.to.17.years))),
      Public.Coverage..Margin.of.Error..6.to.17.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..6.to.17.years))),
      Percent.Public.Coverage..Estimate..6.to.17.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..6.to.17.years))),
      Percent.Public.Coverage..Margin.of.Error..6.to.17.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..6.to.17.years))),
      Total..Estimate..18.to.24.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..18.to.24.years))),
      Total..Margin.of.Error..18.to.24.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..18.to.24.years))),
      Public.Coverage..Estimate..18.to.24.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..18.to.24.years))),
      Public.Coverage..Margin.of.Error..18.to.24.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..18.to.24.years))),
      Percent.Public.Coverage..Estimate..18.to.24.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..18.to.24.years))),
      Percent.Public.Coverage..Margin.of.Error..18.to.24.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..18.to.24.years))),
      Total..Estimate..25.to.34.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..25.to.34.years))),
      Total..Margin.of.Error..25.to.34.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..25.to.34.years))),
      Public.Coverage..Estimate..25.to.34.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..25.to.34.years))),
      Public.Coverage..Margin.of.Error..25.to.34.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..25.to.34.years))),
      Percent.Public.Coverage..Estimate..25.to.34.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..25.to.34.years))),
      Percent.Public.Coverage..Margin.of.Error..25.to.34.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..25.to.34.years))),
      Total..Estimate..35.to.44.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..35.to.44.years))),
      Total..Margin.of.Error..35.to.44.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..35.to.44.years))),
      Public.Coverage..Estimate..35.to.44.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..35.to.44.years))),
      Public.Coverage..Margin.of.Error..35.to.44.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..35.to.44.years))),
      Percent.Public.Coverage..Estimate..35.to.44.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..35.to.44.years))),
      Percent.Public.Coverage..Margin.of.Error..35.to.44.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..35.to.44.years))),
      Total..Estimate..45.to.54.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..45.to.54.years))),
      Total..Margin.of.Error..45.to.54.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..45.to.54.years))),
      Public.Coverage..Estimate..45.to.54.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..45.to.54.years))),
      Public.Coverage..Margin.of.Error..45.to.54.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..45.to.54.years))),
      Percent.Public.Coverage..Estimate..45.to.54.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..45.to.54.years))),
      Percent.Public.Coverage..Margin.of.Error..45.to.54.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..45.to.54.years))),
      Total..Estimate..55.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..55.to.64.years))),
      Total..Margin.of.Error..55.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..55.to.64.years))),
      Public.Coverage..Estimate..55.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..55.to.64.years))),
      Public.Coverage..Margin.of.Error..55.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..55.to.64.years))),
      Percent.Public.Coverage..Estimate..55.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..55.to.64.years))),
      Percent.Public.Coverage..Margin.of.Error..55.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..55.to.64.years))),
      Total..Estimate..65.to.74.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..65.to.74.years))),
      Total..Margin.of.Error..65.to.74.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..65.to.74.years))),
      Public.Coverage..Estimate..65.to.74.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..65.to.74.years))),
      Public.Coverage..Margin.of.Error..65.to.74.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..65.to.74.years))),
      Percent.Public.Coverage..Estimate..65.to.74.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..65.to.74.years))),
      Percent.Public.Coverage..Margin.of.Error..65.to.74.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..65.to.74.years))),
      Total..Estimate..75.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..75.years.and.over))),
      Total..Margin.of.Error..75.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..75.years.and.over))),
      Public.Coverage..Estimate..75.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..75.years.and.over))),
      Public.Coverage..Margin.of.Error..75.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..75.years.and.over))),
      Percent.Public.Coverage..Estimate..75.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..75.years.and.over))),
      Percent.Public.Coverage..Margin.of.Error..75.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..75.years.and.over))),
      Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..MEDICAID.MEANS.TESTED.PUBLIC.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...Under.18))),
      Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...18.to.64.years))),
      Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Total..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=sum(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Estimate..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over=mean(na.rm=TRUE, as.numeric(as.vector(Percent.Public.Coverage..Margin.of.Error..VA.HEALTH.CARE.COVERAGE.ALONE.OR.IN.COMBINATION...65.years.and.over))),
      medicaid.expansion.percent = mean(na.rm=TRUE, as.numeric(as.vector(Total..Estimate..PUBLIC.HEALTH.INSURANCE.ALONE.OR.IN.COMBINATION...Below.138.percent.of.the.poverty.threshold))/as.numeric(as.vector(Total..Estimate..Civilian.noninstitutionalized.population))),
      public.coverage.percent <- mean(na.rm=TRUE, as.numeric(as.vector(Public.Coverage..Estimate..COVERAGE.ALONE...Public.health.insurance.alone)))/as.numeric(as.vector(Total..Estimate..Civilian.noninstitutionalized.population))
      
      ), by= list(districtcode, representative, firstname, middlename, lastname, party, phone, website, congress_office, bioguide_id, votesmart_id, fec_id, govtrack_id, crp_id, twitter_id, congresspedia_url, facebook_id, oc_email)]
      
      
      
      
      
      
      
      cleantable <- superZipInBoundsCollapse %>%
      dplyr::select(
      District = districtcode,
      Representative = representative,
      Party=party,
      Phone = phone,
      Email= oc_email,
      Website = website,
      twitter = twitter_id,
      Rank = rank,
      Score = centile,
      Superzip = superzip,
      Population = adultpop,
      College = college,
      unemployment = Unemp..Rate,
      Income = income,
      Medicaid = medicaid.expansion.percent
      )
      
      
      df <- unique(cleantable)
      
      df
      
  })
  

  
  
  output$congresstable <- DT::renderDataTable({
      
      df <- congressTable()
      DT::datatable(df)


  })
  
  output$downloadcongresstable <- downloadHandler(
  filename = function() { paste("congress", '.csv', sep=',') },
  content = function(file
  ) {
      write.csv(congressTable(), file)
  }
  )
  
  
  
}
