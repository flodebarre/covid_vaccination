plotFig <- function(c1, c2, week, densOrProp, sameScale = FALSE, byRec = 100000, thedate, newdat){
  # c1        character, two-letter country name, country 1
  # c2        character, two-letter country name, country 2
  # week      numeric, week number in 2021
  # densOrProp character, code for whether densities ("popsize") or proportions ("prop")
  # sameScale Boolean, whether to use the same scale on both sides
  # byRec     numeric, graduations for population
  # thedate   character, date of the data
  # newdat    dataframe, dataset
  

  #----------------------------
  # # This is just for testing the function -- comment out when you are done
  # c1 <- "ES"
  # c2 <- "FR"
  # week <- "2021-W37"
  # byRec <- 100000
  # sameScale <- TRUE
  # densOrProp <- "prop"
  #----------------------------
  
  
  # Define colors 
  colPop <- gray(0.9) # Unvaccinated
  colComplet1 <- "#AA3939" # 2 doses, left
  col1D1 <- "#FFAAAA" # 1 dose, left
  colRappel1 <- "#550000" # 3 doses, left
  
  colComplet2 <- "#226666" # 2 doses, right
  col1D2 <- "#9BC2C2" # 1 dose, right
  colRappel2 <- "#003333" # 3 doses, right
  
  # Subset of the data with these countries
  tmp <- newdat[is.element(newdat$ReportingCountry, c(c1, c2)) & (newdat$YearWeekISO == week), ]
  
  # Get max size of age class
  tmp1 <- tmp[tmp$ReportingCountry == c1, ]
  tmp2 <- tmp[tmp$ReportingCountry == c2, ]
  xmax1 <- max(tmp1$Denominator / tmp1$ageWidth, na.rm = TRUE) # Remove potential NAs
  xmax2 <- max(tmp2$Denominator / tmp2$ageWidth, na.rm = TRUE) # Remove potential NAs
  
  xmax <- max(c(xmax1, xmax2))
  
  # Rescale population sizes
  # Sizes by year of age: divide by width of the age class
  if(densOrProp == "popsize"){
    if(sameScale){
      # If same scale, rescale using xmax for both
      tmp1$RDenom <- tmp1$Denominator / xmax / tmp1$ageWidth
      tmp1$R1D <- tmp1$FirstDose / xmax / tmp1$ageWidth
      tmp1$R2D <- tmp1$SecondDose / xmax / tmp1$ageWidth
      tmp1$R3D <- tmp1$DoseAdditional1 / xmax / tmp1$ageWidth
      
      tmp2$RDenom <- tmp2$Denominator / xmax / tmp2$ageWidth
      tmp2$R1D <- tmp2$FirstDose / xmax / tmp2$ageWidth
      tmp2$R2D <- tmp2$SecondDose / xmax / tmp2$ageWidth
      tmp2$R3D <- tmp2$DoseAdditional1 / xmax / tmp2$ageWidth
      
    }else{
      # If different scale, rescale using the maximum of each country data
      tmp1$RDenom <- tmp1$Denominator / xmax1 / tmp1$ageWidth
      tmp1$R1D <- tmp1$FirstDose / xmax1 / tmp1$ageWidth
      tmp1$R2D <- tmp1$SecondDose / xmax1 / tmp1$ageWidth
      tmp1$R3D <- tmp1$DoseAdditional1 / xmax1 / tmp1$ageWidth
      
      tmp2$RDenom <- tmp2$Denominator / xmax2 / tmp2$ageWidth
      tmp2$R1D <- tmp2$FirstDose / xmax2 / tmp2$ageWidth
      tmp2$R2D <- tmp2$SecondDose / xmax2 / tmp2$ageWidth
      tmp2$R3D <- tmp2$DoseAdditional1 / xmax2 / tmp2$ageWidth
    }
  }else{
    # Proportions
    tmp1$RDenom <- 1
    tmp1$R1D <- tmp1$FirstDose / tmp1$Denominator
    tmp1$R2D <- tmp1$SecondDose / tmp1$Denominator
    tmp1$R3D <- tmp1$DoseAdditional1 / tmp1$Denominator
    
    tmp2$RDenom <- 1
    tmp2$R1D <- tmp2$FirstDose / tmp2$Denominator
    tmp2$R2D <- tmp2$SecondDose / tmp2$Denominator
    tmp2$R3D <- tmp2$DoseAdditional1 / tmp2$Denominator
  }
  
  par(xpd = FALSE, family = "sans", mgp = c(2, 0.15, 0), tck = -0.02)
  par(mar = c(5, 2.5, 5.5, 2.5))
  
  # Initialize plot
  plot(c(-1, 1), c(0, 100), type = "n", 
       axes = FALSE, xlab = "", ylab = "", 
       xaxs = "i")
  
  # Write Credits
  par(family = "mono")
  mtext(side = 1, line = 3.5, text = paste0("@flodebarre, adapted from @VictimOfMaths, ", thedate, " 
Data ECDC: https://opendata.ecdc.europa.eu/covid19/vaccine_tracker/csv/data.csv
Code: https://github.com/flodebarre/covid_vaccination/blob/main/ECDC.Rmd"), adj = 0, cex = 0.55, col = gray(0.5))
  par(family = "sans")
  
  # For each country / side of the plot
  for(ictr in c(1, 2)){
    
    # Get the country ID
    ctr <- c(c1, c2)[ictr]
    
    # Define side and colors
    factor <- (-1)^ictr
    colComplet <- get(paste0("colComplet", ictr))
    col1D <- get(paste0("col1D", ictr))
    colRappel <- get(paste0("colRappel", ictr))
    
    tmpp <- get(paste0("tmp", ictr))
    fullCtr <- unique(tmpp$Country) # Full name
    
    for(ag in unique(tmpp$TargetGroup)){
      # For each age class, 
      # Subset of the data
      tmp <- tmpp[tmpp$TargetGroup == ag, ]
      
      # Only plot if there is a size of the age class
      if(!is.na(tmp$RDenom) & !is.na(tmp$R1D)){
        
      # Plot Total population
      rect(xleft = factor * tmp$RDenom, ybottom = tmp$minAge, 
           xright = 0, ytop = tmp$maxAge + 1, 
           col = colPop, border = gray(0, 0))
      
        ## Full vaccination in the end
        # Vaccinated, 1 dose
        rect(xleft = factor * max(0, tmp$RDenom - tmp$R1D), ybottom = tmp$minAge, 
             xright = factor * tmp$RDenom, ytop = tmp$maxAge + 1, 
             col = col1D, border = gray(0, 0))
        
        # Vaccinated, complete
        rect(xleft = factor * max(0, tmp$RDenom - tmp$R2D), ybottom = tmp$minAge, 
             xright = factor * tmp$RDenom, ytop = tmp$maxAge + 1, 
             col = colComplet, border = gray(0, 0))

        # Vaccinated, rappel1
        rect(xleft = factor * max(0, tmp$RDenom - tmp$R3D), ybottom = tmp$minAge, 
             xright = factor * tmp$RDenom, ytop = tmp$maxAge + 1, 
             col = colRappel, border = gray(0, 0))
        
        # Identify whether denominator problem
        if(tmp$RDenom < tmp$R1D){
          # If denominator problem, 
          # flag it with a *
          if(factor == -1){adjj <- 1}else{adjj <- 0}
          par(xpd = TRUE)
          text(x = factor, y = tmp$minAge/2 + (tmp$maxAge + 1)/2, labels = "*", cex = 2, font = 2, adj = c(adjj, 0.5))
          par(xpd = FALSE)
        }
        
      # Graduations for age class
      lines(c(factor, 0), rep(tmp$minAge, 2), col = "white", lwd = 1.5)
      
      } # end if age class size exists
      
      # Age values
      par(xpd = TRUE)
      if(factor == -1){adjj <- 1}else{adjj <- 0}
      text(x = factor, y = tmp$minAge, labels = tmp$minAge, col = gray(0), adj = c(adjj, 0.25), cex = 0.9)
      par(xpd = FALSE)
      
    } # end age class
    # Add country legend
    par(xpd = TRUE)
    if(factor == -1){adjj <- 0}else{adjj <- 1}
    text(x = factor, y = 110, labels = fullCtr, adj = c(adjj, 0), cex = 1.3, font = 2)
    par(xpd = FALSE)
    
  }
  
  cexl <- 0.9 # Text size of legend of axes
  
  # Legend / title
  if(densOrProp == "popsize"){
    t1 <- paste0("Population by year of age
(One rectangle: ", format(byRec, scientific = FALSE)," individuals)
* next to an age band means potential denominator issues in the data")
  }else{
    t1 <- paste0("Proportions by age band
Graduations every 10%
* next to an age band means potential denominator issues in the data")
  }
  
  mtext(t1, side = 1, line = 1.25, cex = cexl)
  
  par(xpd = FALSE)
  # Graduations
  wfine <- 0.3
  # Horizontal by year of age
  # Only if populations size
  if(densOrProp == "popsize"){
    for(i in 0:110){
      lines(c(-1, 1), rep(i, 2), col = "white", lwd = wfine)
    }
  }
  # Vertical by byRec (10000 for instance)
  if(densOrProp == "popsize"){
    for(i in seq(0, 2*10^6, by = byRec)){
      if(sameScale){
        abline(v = i/xmax, col = "white", lwd = wfine)
        abline(v = -i/xmax, col = "white", lwd = wfine)
      }else{
        abline(v = i/xmax2, col = "white", lwd = wfine)
        abline(v = -i/xmax1, col = "white", lwd = wfine)
      }
    }
  }else{
    for(i in seq(0, 1, by = 0.1)){
      abline(v = i, col = "white", lwd = wfine)
      abline(v = -i, col = "white", lwd = wfine)
    }
  }
  # Vertical separation of the two countries
  #abline(v = 0, col = gray(0), lwd = 1.5)
  lines(x = c(0, 0), y = c(0, max(tmpp$maxAge) + 1), col = 1, lwd = 1.5, lend = "butt")
  
  # Legend ages
  par(xpd = TRUE)
  
  yl <- 100   # y position of "Age" 
  text("Age", x = 1, y = yl, cex = cexl, adj = c(0, 1))
  text("Age", x = -1, y = yl, cex = cexl, adj = c(1, 1))
  
  
  # Plot legend (manually centered)
  par(family = "mono")
  legend(x = 0, y = 105, pch = 15, col = c(colRappel1, colComplet1, col1D1, colPop, colRappel2, colComplet2, col1D2, colPop), ncol = 2, legend = c("     3 doses", "     2 doses", "     1 dose", " non vaccinated", "", "", "", ""), xjust = 0.29, yjust = 0, box.lwd = -1, text.font = 1, pt.cex = 2, cex = 0.8, bg = gray(0, 0))
  
  # Add title 
  par(family = "sans")
  mtext(week, side = 3, line = 4, cex = 1.2, font = 2)
  
}
