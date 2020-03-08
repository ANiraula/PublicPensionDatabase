###########
rm(list=ls())
library(shiny)
library(tseries)
library(ggplot2)
library(data.table)
library(readr)
library(ggplot2)


#Or Load csv PPD file from GitHub repository
urlfile="https://github.com/ANiraula/PublicPlansData/blob/master/ppd-data-latest.csv?raw=true"
PPD<-read_csv(url(urlfile), col_names = TRUE, na = c(""), skip_empty_rows = FALSE, col_types = NULL)
PPD <- setDT(PPD)
#View(PPD)
#Filter out Pension Plan
List.all <- unique(PPD, by = c("PlanName"))
#Select only state-level retirement systems
List.state <- List.all[AdministeringGovt==0]
#List$PlanName[1:10]

ui <- fluidPage(
  titlePanel("Choose Public Pension Plan"),
  # CODE BELOW: Add select input named "PlanName" to choose between different pension plans in PPD database
  selectInput("x", "PlanName", selected = "Louisiana Teachers", choices = List.state$PlanName),
  # Add plot output to display Assumed Rates of Return for a Chosen Pension Plan
  plotOutput('plot_ARR')
)

server <- function(input, output, session){
  # Render plot of top 10 most popular names
  output$plot_ARR <- renderPlot({
    #Filter out variable (e.g. "InvestmentReturnAssumption_GASB, "InvestmentReturn_1yr")
    ARR <- data.frame(PPD[PlanName == input$x]$InvestmentReturnAssumption_GASB)
    ARR <- data.frame("Assumed Returns"= c(ARR[,1]),
                     "Years"=c(seq(2001, 2019, by=1)))
    # Plot assumed rate of return for selected plan
    ggplot(data=ARR, aes(x=ARR[,2], y=ARR[,1]), group=1) +
      geom_line(linetype = "longdash", color="blue")+
      geom_point(color="blue") +
      scale_y_continuous(labels = function(x) paste0(x*100, "%"), name = "Assumed Rate of Return",
                         breaks = seq(0.04, 0.1, by = 0.005), limits = c(0.04, 0.1))+
      scale_x_continuous(labels = function(x) paste0(x*1, ""), name = "",
                         breaks = seq(2001, 2019, by = 1), limits = c(2001, 2019))+
      theme_bw() + theme(panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  })
}

shinyApp(ui = ui, server = server)

View(ARR)
