shinyUI(pageWithSidebar(
    headerPanel("Optimised Sample Placement"),
    sidebarPanel(
        sliderInput("addsamp",
                    "Number of samples:",
                    min = 1,
                    max = 100,
                    value = 10),
        selectInput("choice","What are you trying to optimise?",
                    choices=c("Spatial Interpolation","Variogram Esitmation","Trend Identification")),
        p("Note that this will be slower for larger sample numbers, particularly for trend identification.")
    ),
    
    mainPanel(
        tabsetPanel(type="tabs",
                    tabPanel("Map",
                             h3("Mud map of the Meuse River area"),
                             p("Optimised sample locations shown as black dots."),
                             plotOutput(outputId="Sampplot",height = "500px")
                    ),
                    tabPanel("Coordinates",
                             p("Coordinates of optimised sample locations"),
                             tableOutput("locs")
                    ),
                    tabPanel("Documentation",
                             h3("Introduction"),
                             p("This app allows the user to optimise the sample location of additional samples into the Meuse River catchment area. The data sources are the meuse and meuse.grid datasets in R."),
                             p("This area is flooded periodically and the soils are conmatinated with Pb, Zn and Cd."),
                             p("This app uses spatial simulated annealing to best predict the optimised location of sample points considering the distance from the river, the soil type and the flooding frequency."),
                             h3("Optimisation Type"),
                             p("The optimal sample grid can be chosen to optimise variogram estimation, trend identification or spatial interpolation."),
                             p("This app utilises codes within the Spsann package available on CRAN. See documentation internal to that package for further details."),
                             h3("Sample numbers"),
                             p("This app is limited to adding 100 samples,"),
                             p("The more samples you add, the slower the process."),
                             p("The trend identifiaction algorithm can be quite slow for larger sample numbers")
                    )
        )
    )
)
)
