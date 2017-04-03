# DS-501 Case Study 3
# Author: David Beach

# https://www.kaggle.com/lislejoem/us_energy_census_gdp_10-14
# http://berc.berkeley.edu/wp-content/uploads/2013/06/Saxum_Energy_Final-Front.jpg

library(shiny)
library(shinythemes)
library(shinydashboard)
library(data.table)
library(plotly)
library(cluster)

categories.produce = data.table(
  code=c(
    "import",
    "CLPRB",
    "NGMPB",
    "PAPRB",
    "NUETB",
    "EMTCB",
    "WWTCB",
    "GETCB",
    "HYTCB",
    "SOTCB",
    "WYTCB"
  ),
  name=c(
    "Import",
    "Coal",
    "Natural Gas",
    "Crude Oil",
    "Nuclear",
    "Ethanol",
    "Biomass",
    "Geothermal",
    "Hydroelectric",
    "Solar",
    "Wind"
  ),
  color=c(
    "#c0c0c0", # import/export grey
    "#737373", # coal grey
    "#6baed6", # natural gassy blue
    "#807dba", # petroleum purple 4
    "#fc8d59", # nuclear orange
    "#e5f5e0", # renewable green 1
    "#c7e9c0", # renewable green 2
    "#a1d99b", # renewable green 3
    "#74c476", # renewable green 4
    "#41ab5d", # renewable green 5
    "#238b45"  # renewable green 6
  ),
  multiplier=1
)

categories.consume = data.table(
  code = c(
    "export",
    "CLTCB",
    "NNTCB",
    "DFTCB",
    "JFTCB",
    "LGTCB",
    "MMTCB",
    "POTCB2",
    "ESTCB",
    "TETCB"
  ),
  name = c(
    "Export",
    "Coal",
    "Natural Gas",
    "Distillate Fuel",
    "Jet Fuel",
    "Propane",
    "Gasoline",
    "Other Petroleum",
    "Electricity",
    "Total Consumption"
  ),
  color = c(
    "#C0C0C0", # import/export grey
    "#737373", # coal grey
    "#6baed6", # natural gassy blue
    "#dadaeb", # petroleum purple 1
    "#bcbddc", # petroleum purple 2
    "#9e9ac8", # petroleum purple 3
    "#807dba", # petroleum purple 4
    "#6a51a3", # petroleum purple 5
    "#ffeda0", # electric yellow
    "#80c0ff"  # total blue
  ),
  multiplier = -1
)

categories.divisor = data.table(
  name = c(
    "Absolute (Billion BTUs)",
    "per GDP (Billion BTUs / 2016 USD)",
    "per Capita (Billion BTUs / Person)",
    "per Land Area (Billion BTUs / Sq. Mile)"
  ),
  code = c(
    "abs",
    "gdp",
    "population",
    "area"
  ),
  unit = c(
    "Billion BTUs",
    "Billion BTUs per US Dollar (at present value)",
    "Billion BTUs per Person",
    "Billion BTUs per Square Mile"
  )
)

energy.all = data.table(read.csv("cleaned_data/energy_all.csv", header=T, stringsAsFactors=F))


colorCheckboxGroup = function(id, label, cats, selected=c())  {
  column(6,
         lapply(1:nrow(cats), function(x) {
           col <- cats$color[x]
           css_col <- paste0("#", id, " div.checkbox:nth-child(",x,
                             ") span{background-color: ", col,";}")
           tags$style(type="text/css", css_col)
         }),
         checkboxGroupInput(id, label,
                            choices = setNames(as.list(cats$code), cats$name),
                            selected = selected
                            ),
         tags$style(type="text/css", paste0("#", id, "Clear", " { height: 20px; padding-top: 0px; padding-bottom: 0px; padding-left: 5px; padding-right: 5px }")),
         actionButton(inputId=paste0(id, "Clear"), label="Clear", width=50)
  )
}

buildControls <- function() {
    div(

      img(src="EDemandOverview.png", width="100%"),
      
      sliderInput(inputId="year",
                  label="Year Range",
                  min = 1960,
                  max = 2014,
                  value = c(2000, 2014),
                  sep = ""),
      
      fluidRow(
        colorCheckboxGroup("consumption", "Consumption", categories.consume[code!="TETCB"]),
        colorCheckboxGroup("production", "Production", categories.produce,
                           selected = c("NUETB", "CLPRB", "NGMPB", "PAPRB")
        )
      ),
      
      # add extra vertical space here
      br(),
      
      sliderInput(inputId="numGroups",
                  label="Group by Energy Profile",
                  min = 1,
                  max = 8,
                  value = 3,
                  sep = ""),
      
      selectInput(inputId="divisor",
                  label="Energy Scale",
                  choices=setNames(categories.divisor$code, categories.divisor$name)
      )

    )
}

# Define UI for application that draws a histogram
ui <- dashboardPage(

   skin="yellow",
   
   # Application title
   dashboardHeader(title="The 50 States of US Energy", titleWidth=300),
   dashboardSidebar(width=300,
     sidebarMenu(
       menuItem("Composition", tabName="composition", icon=icon("bar-chart")),
       menuItem("Time Series", tabName="timeseries", icon=icon("area-chart")),
       menuItem("User's Guide", tabName="guide", icon=icon("life-bouy")),
       menuItem("Lab Notebook", tabName="notebook", icon=icon("book"))
     ),
     br(),
     buildControls()
   ),

   dashboardBody(
     tabItems(
       tabItem("composition", plotlyOutput("compositionPlot", height="1200px", width="100%")),
       tabItem("timeseries", plotlyOutput("timeseriesPlot", height="1200px", width="100%")),
       tabItem("guide", includeMarkdown("www/UserGuide.Rmd")),
       tabItem("notebook", includeMarkdown("EnergyNotebook.Rmd"))
     )
   )
)


ribbonPlot = function(X, cats, divisor="abs", title="") {
  data <- X

  if(divisor == "abs") {
    divcol = 1
  } else {
    divcol = data[, get(divisor)]
  }

  cumpos = rep(0, nrow(data))
  cumneg = rep(0, nrow(data))
  for(i in nrow(cats):1) {
    code = cats$code[i]
    if(cats$multiplier[i] > 0) {
      data[, code] = cumpos + data[, get(code)]
      cumpos = data[, get(code)]
    } else {
      data[, code] = cumneg + data[, get(code)]
      cumneg = data[, get(code)]
    }
  }
  
  code <- cats$code[1]
  name <- cats$name[1]
  color <- cats$color[1]
  multiplier <- cats$multiplier[1]
  plot <- plot_ly(data,
                  x=signif(data[, get(code)] * multiplier / divcol, 3),
                  y=~year,
                  name=name,
                  type="area",
                  mode="none",
                  fill="tozerox",
                  fillcolor=color,
                  hoverinfo="x+text"
  )

  if(nrow(cats) >= 2) { 
    for(i in 2:nrow(cats)) {
      code <- cats$code[i]
      name <- cats$name[i]
      color <- cats$color[i]
      multiplier <- cats$multiplier[i]
      plot <- plot %>% add_trace(
        x=signif(data[, get(code) * multiplier / divcol], 3),
        type="area",
        name=name,
        fillcolor=color,
        hoverinfo="x+text"
      )
    }
  }
  
  plot <- plot %>% layout(
    showlegend=F,
    yaxis=list(autorange="reversed"),
    xaxis=list(title="")
  )

  plot
  
}



tornadoPlot = function(X, cats, divisor="abs", title="") {
  data <- X

  stateFont <- list(family="Helvetica", size=14, weight="bold")
  
  data$total <- 0
  for(i in 1:nrow(cats)) {
    code <- cats$code[i]
    data$total <- data$total + data[, get(code)]
  }
  if(divisor != "abs") {
    data$total <- (data$total / data[, get(divisor)])
  }
  
  data <- data[order(data$total, decreasing=T)]
  data$state = factor(data$state, levels=data$state[length(data$state):1])

  if(divisor == "abs") {
    divcol = 1
  } else {
    divcol = data[, get(divisor)]
  }
  
  code <- cats$code[1]
  name <- cats$name[1]
  color <- cats$color[1]
  multiplier <- cats$multiplier[1]
  plot <- plot_ly(data,
                  y=~state,
                  x=signif(data[, get(code)] * multiplier / divcol, 3),
                  name=name,
                  marker=list(color=color),
                  type="bar",
                  orientation="h")

  if(nrow(cats) >= 2) { 
    for(i in 2:nrow(cats)) {
      code <- cats$code[i]
      name <- cats$name[i]
      color <- cats$color[i]
      multiplier <- cats$multiplier[i]
      plot <- plot %>% add_trace(
        x=signif(data[, get(code) * multiplier / divcol], 3),
        name=name,
        marker=list(color=color)
      )
    }
  }
  
  plot <- plot %>% layout(
    showlegend=F,
    yaxis=list(title="", tickfont=stateFont),
    xaxis=list(title="",
               zerolinecolor="#808080",
               zerolinewidth=2,
               ticks="outside",
               showline=T),
    barmode="relative") %>% layout(annotations=list(
      list(x=0.0, y=1.0, text=paste0("<b>", title, "</b>"), showarrow=F,
           xref='paper', yref='paper', yanchor='bottom')
    ))

  return(plot)
}

do_cluster <- function(df, cats, centers, divisor) {

  total = 0
  for(cat in cats) {
    total = total + df[, get(cat)]
  }
  cluster.df = data.table(state=df$state)
  for(cat in cats) {
    cluster.df[, cat] = df[, get(cat)] / ifelse(total > 0, total, 1)
  }
  cluster.df$state = NULL
  
  clus = kmeans(cluster.df, centers=centers)

  cluster.df$cluster = clus$cluster
  cluster.df$total = total
  if(divisor != "abs") {
    cluster.df$total = cluster.df$total / df[, get(divisor)]
  }
  
  
  cluster.priority = cluster.df[, .(size=max(total)), by=.(cluster)]
  cluster.priority = cluster.priority[order(cluster.priority$size, decreasing=T)]

  clusters = factor(cluster.df$cluster, levels=cluster.priority$cluster)

  clusters
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
    observeEvent(input$productionClear, {
      updateCheckboxGroupInput(session, "production", selected=character(0))
    })

    observeEvent(input$consumptionClear, {
      updateCheckboxGroupInput(session, "consumption", selected=character(0))
    })
  
    output$compositionPlot <- renderPlotly({

     year.range <- input$year
     year.first <- year.range[1]
     year.last <- year.range[2]
     
     pcodes <- input$production
     ccodes <- input$consumption

     # need at least two codes for multiple cluster
     codes = c(pcodes, ccodes)
     if(length(codes) == 0) {
       ccodes = c("TETCB")
       codes = c("TETCB")
     }
     
     categories <- rbindlist(list(
       categories.produce[is.element(code, pcodes)],
       categories.consume[is.element(code, ccodes)]
     ))
     
     df = energy.all[year >= year.first & year <= year.last, .(
        state.name = first(state.name),
        population = sum(population), 
        gdp = sum(gdp),
        area = first(area),
        latitude = first(latitude),
        longitude = first(longitude),
        NUETB = sum(NUETB),
        CLTCB = sum(CLTCB),
        NNTCB = sum(NNTCB),
        DFTCB = sum(DFTCB),
        JFTCB = sum(JFTCB),
        LGTCB = sum(LGTCB),
        MMTCB = sum(MMTCB),
        POTCB2 = sum(POTCB2),
        ESTCB = sum(ESTCB),
        TETCB = sum(TETCB),
        EMTCB = sum(EMTCB),
        WWTCB = sum(WWTCB),
        GETCB = sum(GETCB),
        HYTCB = sum(HYTCB),
        SOTCB = sum(SOTCB),
        WYTCB = sum(WYTCB),
        CLPRB = sum(CLPRB),
        NGMPB = sum(NGMPB),
        PAPRB = sum(PAPRB),
        export = sum(export),
        import = sum(import)
    ), by=.(state)]
    
     clusters.num = input$numGroups
       
     if(length(codes) <= 1) {
       clusters.num = 1
     }
     
     if(clusters.num > 1) {
       df$cluster = do_cluster(df, c(pcodes, ccodes), clusters.num, input$divisor)
     } else {
       df$cluster = factor(1, levels=c(1))
     }

     priority = levels(df$cluster)

     plots <- vector("list", clusters.num)
     heights = c()
     for(c in 1:clusters.num) {
         subframe <- df[cluster==priority[c]]
         plots[[c]] <- tornadoPlot(subframe, categories, divisor=input$divisor, title=paste("Group", c))
         heights = append(heights, nrow(subframe) + 2)
     }
     heights <- heights / sum(heights)
     
     subplot(plots,
             nrows=clusters.num,
             shareX=T,
             titleX=T,
             heights=heights,
             margin=c(0, 0, 0.01, 0.01),
             which_layout=c(clusters.num)
     )
     
   })

   output$timeseriesPlot <- renderPlotly({

     year.range <- input$year
     year.first <- year.range[1]
     year.last <- year.range[2]
     
     pcodes <- input$production
     ccodes <- input$consumption
     
     # need at least two codes for multiple cluster
     codes = c(pcodes, ccodes)
     if(length(codes) == 0) {
       ccodes = c("TETCB")
       codes = c("TETCB")
     }
     
     categories <- rbindlist(list(
       categories.produce[is.element(code, pcodes)],
       categories.consume[is.element(code, ccodes)]
     ))
     
     df = energy.all[year >= year.first & year <= year.last, .(
       #state.name = first(state.name),
       population = sum(population), 
       gdp = sum(gdp),
       area = first(area),
       #latitude = first(latitude),
       #longitude = first(longitude),
       NUETB = sum(NUETB),
       CLTCB = sum(CLTCB),
       NNTCB = sum(NNTCB),
       DFTCB = sum(DFTCB),
       JFTCB = sum(JFTCB),
       LGTCB = sum(LGTCB),
       MMTCB = sum(MMTCB),
       POTCB2 = sum(POTCB2),
       ESTCB = sum(ESTCB),
       TETCB = sum(TETCB),
       EMTCB = sum(EMTCB),
       WWTCB = sum(WWTCB),
       GETCB = sum(GETCB),
       HYTCB = sum(HYTCB),
       SOTCB = sum(SOTCB),
       WYTCB = sum(WYTCB),
       CLPRB = sum(CLPRB),
       NGMPB = sum(NGMPB),
       PAPRB = sum(PAPRB),
       export = sum(export),
       import = sum(import)
     ), by=.(year)]
     
     ribbonPlot(df, categories)
          
   })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

