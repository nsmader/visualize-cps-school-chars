

## Set up environment
  rm(list = ls())
  #library("lattice")
  library("ggplot2")
  library("car")
  library("shiny")
  #setwd("/home/nsmader/cps-compare-pairs")
  setwd("~/../Google Drive/Thrive Chicago - Engaged in enrichment and academic activities/Data on Schools/cps-compare-pairs-elem")
  
  f.to.c <- function(f){ return(levels(f)[f]) }
  f.to.n <- function(f){ return(as.numeric(levels(f)[f])) }
  cn <- function(x) colnames(x)
  "%&%" <- function(...) paste0(...)
  
## Load data
  elem <- read.csv("./elem_data.csv", stringsAsFactors = F)
  colnames(elem) <- c("schlid", "schlname", "st.addr", "city", "state", "zip", "phone", "network",
                      "NWEA.attnVsAvg.read", "NWEA.attnVsAvg.math", "NWEA.growthVsAvg.read", "NWEA.growthVsAvg.math",
                      "ISAT.me.3rdread", "ISAT.me.5thcomp", "bil_pct", "sped_pct", "frl_pct", "race_white", "race_afam",
                      "race_hisp", "perform_lvl", "perform_status", "MVMS_overall", "MVMS_family", "MVMS_leaders", 
                      "MVMS_safe", "MVMS_facilities",
                      "NWEA.growthPtl.read", "NWEA.growthPtl.math", "NWEA.attnPtl.read", "NWEA.attnPtl.math",
                      "suspn_per100", "suspn_perMisconduct", "avg_suspn_length", "attend_rate", "oneyear_dropout_rate",
                      "CSI")
  fToConv <- c(grep("^NWEA.+Vs", cn(elem), value = T), "ISAT.me.3rdread", "ISAT.me.5thcomp", grep("pct$", cn(elem), value = T),
               grep("^race", cn(elem), value = T), "suspn_perMisconduct", "attend_rate")
  
  elem[, fToConv] <- sapply(fToConv, function(x) {
    v <- gsub("%", "", elem[, x])
    v[v == "-"] <- NA
    return(as.numeric(v)/100)
  })
  elem$all <- factor(1)
  
  # Remove outlier values
  elem$suspn_per100[elem$suspn_per100 > 100] <- NA
  
  # Relevel factors
  elem$perform_lvl <- factor(elem$perform_lvl, levels = c("LEVEL 1", "LEVEL 2", "LEVEL 3", "NOT ENOUGH DATA"), ordered = T)
  elem$perform_status <- factor(elem$perform_status, c("NOT ON PROBATION", "ON PROBATION", "NOT APPLICABLE"), ordered = T)
  elem$MVMS_overall <- factor(elem$MVMS_overall,
                               c("NOT YET ORGANIZED", "PARTIALLY ORGANIZED", "MODERATELY ORGANIZED", "ORGANIZED", "WELL-ORGANIZED", "NOT ENOUGH DATA"), ordered = T)
  
  MVMSvars <- c("MVMS_safe", "MVMS_family", "MVMS_leaders", "MVMS_facilities")
  for (m in MVMSvars){
    elem[, m] <- factor(elem[, m], c("VERY WEAK", "WEAK", "NEUTRAL", "STRONG", "VERY STRONG", "NOT ENOUGH DATA"), ordered = T)
  }
  
  myLabels <- list("% Stud NWEA Attnm't > Avg - Reading" = "NWEA.attnVsAvg.read",
                     "% Stud NWEA Attnm't > Avg - Math" = "NWEA.attnVsAvg.math",
                     "% Stud NWEA Growth > Avg - Reading" = "NWEA.growthVsAvg.read",
                     "% Stud NWEA Growth > Avg - Math" = "NWEA.growthVsAvg.math",
                     "Pct'l of NWEA Attnm't - Reading" = "NWEA.attnPtl.read",
                     "Pct'l of NWEA Attnm't - Math" = "NWEA.attnPtl.math",
                     "Pct'l of NWEA Growth  - Reading" ="NWEA.growthPtl.read",
                     "Pct'l of NWEA Growth - Math" = "NWEA.growthPtl.math",
                     "% Stud ISAT Meets/Exceeds - 3rd Gr Reading" = "ISAT.me.3rdread", 
                     "% Stud ISAT Meets/Exceeds - 5th Gr Composite" = "ISAT.me.5thcomp",
                     "Stud Attendance Rate" = "attend_rate",
                     "% Free/Reduced Price Lunch" = "frl_pct",
                     "% Race = White" = "race_white",
                     "% Race = African American" = "race_afam",
                     "% Race = Hispanic" = "race_hisp",
                     "% Bilingual" = "bil_pct",
                     "% Special Ed" = "sped_pct",
                     "Suspensions per 100 Youth" = "suspn_per100",
                     "Suspensions per Misconduct" = "suspn_perMisconduct",
                     "Avg. Suspension Length" = "avg_suspn_length",
                     "Community School" = "CSI")
  
  
#------------------------------#
#------------------------------#
# GENERATE SHINY SERVER SCRIPT #
#------------------------------#
#------------------------------#
  
# For troubleshooting, outside of deploying app, can run the following in lieu of getting input from the app: 
#    input <- list(b="cohort", e = AllCondEds, g = "All", r = "All", stringsAsFactors = F); w <- "NoW"
  
shinyServer(function(input, output){
    
  output$pairs <- renderPlot({
    if (length(input$vars) == 1) {
      myPlot <- ggplot(data = elem, aes_string(x = input$vars)) + geom_density(aes_string(colour = input$f)) + 
        ggtitle("Smoothed Histogram Plot for " %&% names(myLabels)[myLabels %in% input$vars]) +
        xlab(names(myLabels)[myLabels %in% input$vars]) +
        ylab("<----- smaller proportion,   larger proportion ----->") + 
        theme(plot.title = element_text(size = 20), axis.title = element_text(size = 15))
      print(myPlot) 
    }
    if (length(input$vars) >= 2) {
      myPlot <- scatterplotMatrix(as.formula(paste0("~", paste(input$vars, collapse="+"), "|", input$f)), data = elem,
                                 var.labels = names(myLabels)[myLabels %in% input$vars],
                                 reg.line = FALSE, lwd = 2)
      print(myPlot) 
    }
  })
  output$densNote <- renderUI({
    if (length(input$vars) == 1){
      HTML("The above plot is a smoothed histogram which shows which values of the characteristic have a relatively high
           or low number of schools.")
    }
  })
  output$pairsNote <- renderUI({
    if (length(input$vars) >= 2) {
      HTML("The matrix above simultaneously shows a series of &ldquo;X-Y&rdquo; plots, as defined by the characteristics along the 
            &ldquo;diagonal&rdquo;. The &ldquo;X&rdquo; for a given plot is the characteristic on the diagonal either above or below the plot, 
            and the &ldquo;Y&rdquo; is the characteristic either to the left or the right of the plot. The scales for each characteristic are 
            indicated on the outside of the matrix.<br>
            <br>
            Within each plot, each dot represents where a CPS elementary school falls in the combination of the X and Y characteristics 
            that define that plot. The lines in the plot show several things--the green line is a  &ldquo;best-fit&rdquo; line going through the 
            scatter plot of schools, the red line is a smoothed &ldquo;best-fit&rdquo; line, and the dashed red lines show approximately 
            the upper and lower limits that contain 90% of the points. The &ldquo;Categorical field for grouping schools&rdquo; drop-down menu at the top left can be used 
            to differentiate groups of schools by giving each dot its own color and shape, based on what categorical field was selected.<br>
            <br>
            The plots along the diagnoal show how the values of that characteristic are distributed. The plotted line is a smoothed 
            histogram which shows which values of the characteristic have a relatively high or low number of schools.<br>
            <br>
            Characteristics which are highly correlated--those which cluster closely around a positively or negatively sloped line--
            would be redundant if both used to target schools.<br>
            Characteristics which are not highly correlated--those which are less of a line, and more of a cloud--would be complementary
            if used to target schools, since they both work to distinguish different types of schools.")
    }
  })
  output$intro <- renderUI({
    if (length(input$vars) < 1){
      HTML("This data visualization tool shows where schools fall along a combination of academic,
            demographic, and organizational rating characteristics of interest to the user. Specifically,
            this tool aims to help target which schools should be pursued for surveys or interviews. By
            selecting characteristics that are relevant to the targeting exercise, users can see which 
            combinations of characteristics are redundant (because they are highly correlated), and which
            are complementary (because they are not correlated, indicating that they measure distinctive things).<br><br>
     
            Select at least one or more schools characteristics at the left to examine how schools compare.")
    }
  })
  
})
  
  
  