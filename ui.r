library(leaflet)

# Choices for drop-downs
varsColor <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Unemployment Rate" = "unemployment",
  "Public Coverage" = "pubcov",
  "Medicare Coverage" = "medicare",
  "Medicaid ACA Coverage" = "medicaidexpansion",
  "VA Coverage" = "va",
  "White %" = "X2010.White",
  "Hispanic %" = "X2010.Hispanic",
  "Black %" = "X2010.Black",
  "Asian %" = "X2010.Asian",
  "Native American %" = "X2010.Native",
  "Trump Voter %" = "X2016.President.Trump.percent",
  "Congress Republican Voter % "= "X2016.House.Rep.percent",
  "Congress Democrat Voter % "= "X2016.House.Dem.percent",
  "Population" = "adultpop"
)

varsSize <- c(
  "Is SuperZIP?" = "superzip",
  "Centile score" = "centile",
  "College education" = "college",
  "Median income" = "income",
  "Unemployment Rate" = "unemployment",
  "Public Coverage" = "pubcov",
  "Medicare Coverage" = "medicare",
  "Medicaid ACA Coverage" = "medicaidexpansion",
  "VA Coverage" = "va",
  "White %" = "X2010.White",
  "Hispanic %" = "X2010.Hispanic",
  "Black %" = "X2010.Black",
  "Asian %" = "X2010.Asian",
  "Native American %" = "X2010.Native",
  "Population" = "adultpop"
)


navbarPage("America at a Glance", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Congressional Explorer"),

        checkboxInput("fullmodel", "Full", value=FALSE),

        tags$hr(),


        textInput("yourzipcode", "Zip Code", value="87108"),

        tags$hr(),

        selectInput("color", "Color", varsColor, selected="college"),
        selectInput("size", "Size", varsSize, selected = "income"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      ),

      tags$div(id="cite",
        'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=100, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=100, value=100)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  ),

tabPanel("Congressional Explorer",
fluidRow(
column(3,
selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
),
column(3,
conditionalPanel("input.states",
selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
)
),
column(3,
conditionalPanel("input.states",
selectInput("districtcodes", "District", c("All Districts"=""), multiple=TRUE)
)
)
),
fluidRow(
column(1,
numericInput("minScore", "Min score", min=0, max=100, value=0)
),
column(1,
numericInput("maxScore", "Max score", min=0, max=100, value=100)
),
column(1,
downloadButton(outputId="downloadcongresstable", label="Download"))
),
hr(),
DT::dataTableOutput("congresstable")
),







  conditionalPanel("false", icon("crosshair"))
)
