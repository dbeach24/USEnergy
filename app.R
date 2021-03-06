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
    "CLPRB",
    "NGMPB",
    "PAPRB",
    "NUETB",
    "HYTCB",
    "WWTCB",
    "EMTCB",
    "WYTCB",
    "GETCB",
    "SOTCB",
    "import"
  ),
  name=c(
    "Coal",
    "Natural Gas",
    "Crude Oil",
    "Nuclear",
    "Hydroelectric",
    "Biomass",
    "Ethanol",
    "Wind",
    "Geothermal",
    "Solar",
    "Import"
  ),
  color=c(
    "#737373", # coal grey
    "#6baed6", # natural gassy blue
    "#7a0177", # petroleum purple 1
    "#fc8d59", # nuclear orange
    "#005a32", # renewable green 1
    "#238443", # renewable green 2
    "#41ab5d", # renewable green 3
    "#78c679", # renewable green 4
    "#addd8e", # renewable green 5
    "#d9f0a3", # renewable green 6
    "#c0c0c0"  # import/export grey
  ),
  multiplier=1
)

categories.consume = data.table(
  code = c(
    "CLTCB",
    "NNTCB",
    "MMTCB",
    "DFTCB",
    "POTCB2",
    "JFTCB",
    "LGTCB",
    "ESTCB",
    "export",
    "TETCB"
  ),
  name = c(
    "Coal",
    "Natural Gas",
    "Gasoline",
    "Distillate Fuel",
    "Other Petroleum",
    "Jet Fuel",
    "Propane",
    "Electricity",
    "Export",
    "Total Consumption"
  ),
  color = c(
    "#737373", # coal grey
    "#6baed6", # natural gassy blue
    "#7a0177", # petroleum purple 1
    "#ae017e", # petroleum purple 2
    "#dd3497", # petroleum purple 3
    "#f768a1", # petroleum purple 4
    "#fa9fb5", # petroleum purple 5
    "#ffeda0", # electric yellow
    "#C0C0C0", # import/export grey
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


ribbonPlot = function(X, cats, divisor="abs", title="", minX=NA, maxX=NA) {
  data <- X

  if(divisor == "abs") {
    divcol = 1
  } else {
    divcol = data[, get(divisor)]
  }

  cats <- cats[nrow(cats):1]
  
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
  xvals = data[, get(code)] * multiplier / divcol
  plot <- plot_ly(data,
                  x=xvals,
                  y=~year,
                  name=name,
                  type="scatter",
                  mode="none",
                  fill="tozerox",
                  fillcolor=color,
                  hoverinfo="x+text"
  )
  
  gxmin = min(xvals)
  gxmax = max(xvals)

  if(nrow(cats) >= 2) { 
    for(i in 2:nrow(cats)) {
      code <- cats$code[i]
      name <- cats$name[i]
      color <- cats$color[i]
      multiplier <- cats$multiplier[i]

      xvals = data[, get(code) * multiplier / divcol]
      gxmin = min(gxmin, xvals)
      gxmax = max(gxmax, xvals)
      
      plot <- plot %>% add_trace(
        x=xvals,
        y=~year,
        name=name,
        type="scatter",
        mode="none",
        fill="tozerox",
        fillcolor=color,
        hoverinfo="x+text"
      )
    }
  }
  
  xaxis=list(title="")
  if(!is.na(minX)) {
    xaxis$range = c(minX, maxX)
  }
  
  plot <- plot %>% layout(
    showlegend=F,
    yaxis=list(autorange="reversed"),
    xaxis=xaxis
  )

  list(plot=plot, gxmin=gxmin, gxmax=gxmax)
  
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

do_cluster <- function(df, cats, centers, divisor, seed=42, nstart=10) {

  set.seed(seed)
  
  total = 0
  for(cat in cats) {
    total = total + df[, get(cat)]
  }
  cluster.df = data.table(state=df$state)
  for(cat in cats) {
    cluster.df[, cat] = df[, get(cat)] / ifelse(total > 0, total, 1)
  }
  cluster.df$state = NULL
  
  clus = kmeans(cluster.df, centers=centers, nstart=nstart)

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
     
     clusters.num = input$numGroups
     
     if(length(codes) <= 1) {
       clusters.num = 1
     }
     
     clusterdf = energy.all[year >= year.first & year <= year.last, .(
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
     
     if(clusters.num > 1) {
       clusterdf$cluster = do_cluster(clusterdf, c(pcodes, ccodes), clusters.num, input$divisor)
     } else {
       clusterdf$cluster = factor(1, levels=c(1))
     }
     
     categories <- rbindlist(list(
       categories.produce[is.element(code, pcodes)],
       categories.consume[is.element(code, ccodes)]
     ))

     xmin <- 0
     xmax <- 0

     for(c in 1:clusters.num) {
       
       states <- clusterdf[cluster==c]$state
       
       df <- energy.all[year >= year.first
                        & year <= year.last
                        & is.element(state, states), .(
                          population = sum(population),
                          gdp = sum(gdp),
                          area = sum(area),
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
       
       plotdata <- ribbonPlot(df, categories, divisor=input$divisor)
       xmin <- min(xmin, plotdata$gxmin)
       xmax <- max(xmax, plotdata$gxmax)
       
     }
          
     plots <- vector("list", clusters.num)
     
     for(c in 1:clusters.num) {
       
       states <- clusterdf[cluster==c]$state

       df <- energy.all[year >= year.first
                       & year <= year.last
                       & is.element(state, states), .(
         population = sum(population),
         gdp = sum(gdp),
         area = sum(area),
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
       
       plotdata <- ribbonPlot(df, categories, divisor=input$divisor, minX=xmin, maxX=xmax)
       
       plots[[c]] <- plotdata$plot

     }

     subplot(plots, nrows=1, shareY=T)

   })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

