library(shiny)

# Define UI for displaying conditional education trends for given populations
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Tool for Comparing Elementary School Characteristics in CPS"),
  
  # Sidebar with controls for demographic
   sidebarPanel(
    selectInput("f", "Categorical field for grouping schools:",
                list("No Separate Groupings"        = "all",
                     "MVMS - Overall Score"         = "MVMS_overall",
                     "MVMS - Involved Families"     = "MVMS_family",
                     "MVMS - Effective Leaders"     = "MVMS_leaders",
                     "MVMS - Safety"                = "MVMS_safe",
                     "MVMS - Quality of Facilities" = "MVMS_facilities",
                     "Performance Level"            = "perform_lvl",
                     "Performance Status"           = "perform_status",
                     "Community School Indicator"   = "CSI"
                     )),
    checkboxGroupInput("vars", "Characteristics to compare:",
                   c("% Stud NWEA Attnm't > Avg - Reading" = "NWEA.attnVsAvg.read",
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
                     "Avg. Suspension Length" = "avg_suspn_length"),
                     ) #selected = c("NWEA.attnVsAvg.read", "NWEA.attnVsAvg.math")
   ),
  
   mainPanel(
    
    htmlOutput("intro"),
    plotOutput("pairs"),
    htmlOutput("densNote"),
    htmlOutput("pairsNote")
   )
  
))

