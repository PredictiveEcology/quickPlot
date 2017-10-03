test_that("Plotting types without visual checking works",{
  r1 <- raster(xmn = 0, xmx = 10, ymn = 0, ymx = 10,
                      vals = sample(1:4, replace = TRUE, size = 100), res = 1)
  
  r2 <- raster(xmn = 0, xmx = 10, ymn = 0, ymx = 10,
               vals = sample(1:4, replace = TRUE, size = 100), res = 1)
  
  s1 <- stack(r1,r2)
  sp1 <- SpatialPoints(cbind(x = stats::runif(10, -50, 50),
                                         y = stats::runif(10, -50, 50)))
  
  if(interactive()) {
    clearPlot()
    expect_silent(Plot(t1)) #agentMatrix
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==1)
    expect_true(length(a$curr@quickPlotGrobList)==1)
    
    expect_silent(Plot(sp1))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==2)
    expect_true(length(a$curr@quickPlotGrobList)==2)
    
    expect_silent(Plot(w1, axes = TRUE, new=TRUE))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==3)
    expect_true(length(a$curr@quickPlotGrobList)==3)
    expect_true(length(a$curr@quickPlotGrobList$w1)==1)
    
    expect_silent(Plot(t1, addTo = "w1"))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==3)
    expect_true(length(a$curr@quickPlotGrobList)==3)
    expect_true(length(a$curr@quickPlotGrobList$w1)==2)
    
    expect_silent(Plot(w2))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==4)
    expect_true(length(a$curr@quickPlotGrobList)==4)
    expect_true(length(a$curr@quickPlotGrobList$w2)==1)
    
    expect_silent(Plot(a1))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==6)
    expect_true(length(a$curr@quickPlotGrobList)==6)
    expect_true(length(a$curr@quickPlotGrobList$`a1$w1`)==1)
    
    expect_silent(Plot(t1, addTo = "a1$w1"))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==6)
    expect_true(length(a$curr@quickPlotGrobList)==6)
    expect_true(length(a$curr@quickPlotGrobList$`a1$w1`)==2)
    
    expect_silent(Plot(s1))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==8)
    expect_true(length(a$curr@quickPlotGrobList)==8)
    expect_true(length(a$curr@quickPlotGrobList$`a1$w1`)==2)
    
    expect_silent(Plot(r1))
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==9)
    expect_true(length(a$curr@quickPlotGrobList)==9)
    expect_true(length(a$curr@quickPlotGrobList$`a1$w1`)==2)
    
    s1$layer.1[3] <- 15
    s1$layer.2[3] <- 25
    
    expect_silent(Plot(s1$layer.1)) # doesn't change s1$layer.2
    expect_silent(Plot(s1$layer.2)) # doesn't change s1$layer.1
    expect_error(Plot(a1[["w1"]])) # doesn't work
    
    #clearPlot()
    #Plot(a1, t1)
    expect_silent(rePlot())
    a <- .getQuickPlot(paste0("quickPlot", dev.cur()))
    expect_true(length(a$isBaseLayer)==9)
    expect_true(length(a$curr@quickPlotGrobList)==9)
    
  }
})
