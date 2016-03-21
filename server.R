library(datasets)
library("spsann")
library("sp")
library("rgeos")
data("meuse.grid")
Boundary <- meuse.grid
sp::coordinates(Boundary) <- c("x", "y")
sp::gridded(Boundary) <- TRUE
Boundary <- rgeos::gUnionCascaded(as(Boundary, "SpatialPolygons"))
candid <- meuse.grid[, 1:2]
covarss <- meuse.grid[, 5:7]
Schedule <- scheduleSPSANN(initial.temperature = 0.5)
Schedule2 <- scheduleSPSANN(chains = 1, initial.temperature = 30,
                            x.max = 1540, y.max = 2060, x.min = 0,
                            y.min = 0, cellsize = 40)
Schedule3 <- scheduleSPSANN(initial.temperature = 5, chains = 1, x.max = 1540, 
                            y.max = 2060,x.min = 0, y.min = 0, cellsize = 40)

shinyServer(function(input, output) {
    
    resis<-reactive({
        if(input$choice=="Trend Identification"){
            optimDIST(points = input$addsamp, candi = candid, covars = covarss, use.coords = TRUE,
                      schedule = Schedule, plotit = FALSE, boundary = boundary)
        } else if (input$choice=="Variogram Esitmation"){
            optimPPL(points = input$addsamp, candi = candid, schedule = Schedule2, lags=7, lags.type = "exponential")
        } else {
            optimCORR(points = input$addsamp, candi = candid, covars = covarss, use.coords = TRUE, schedule = Schedule3)
        }
    })
    
    output$Sampplot<-renderPlot({
        plot(resis(), boundary=Boundary)
    })
    output$locs<-renderTable({
        resis()$points[,2:3]
    })
})
