## Route Optimization
## by Joshua Mire


## libraries

library(shiny)
library(shinybusy)
library(googleway)
library(combinat)

api_set <- 1

## UI

ui <- 
  shinyUI(
    fluidPage(
      add_busy_spinner(spin = "double-bounce", position = "full-page", color = "#4472C4"),
      tags$head(HTML("<title>Route Optimization by Joshua Mire</title>")),
      tags$script(HTML("
        $(document).ready(function() {
          $('.btn').on('click', function(){$(this).blur()});
        })
      ")),
      tags$script(HTML(
        '
          $(document).keyup(function(event) {
            if ($("#api").is(":focus") && (event.key == "Enter")) {
              $("#ok").click();
             }
          })
        '
      )),
      tags$script(HTML(
        '
          $(document).keyup(function(event) {
            if (($("#txtInput1").is(":focus") || $("#txtInput2").is(":focus") || $("#txtInput3").is(":focus") || $("#txtInput4").is(":focus") || $("#txtInput5").is(":focus") || $("#txtInput6").is(":focus") || $("#txtInput7").is(":focus") || $("#txtInput8").is(":focus") || $("#txtInput9").is(":focus")) &&  (event.key == "Enter")) {
              $("#getRoute").click();
             }
          })
        '
      )),
      titlePanel(
        fluidRow(
          column(
            width = 10,
            div(
              h1("Route Optimization"),
              div(
                p("Find the quickest route between up to ten locations."),
                style = "font-size: 14pt; font-style: italic;"
              ),
              style = "padding-left: 20px;"
            )
          ),
          column(
            width = 2,
            div(
              tagList(
                "Created by", 
                a("Joshua Mire", href = "https://www.joshmire.com/", target = "_blank")
              )
            ),
            style = "font-size: 10pt; text-align: right;"
          )
        )
      ),
      sidebarLayout(
        sidebarPanel(
          div(
            fluidRow(
              column(
                width = 8,
                fluidRow(
                  div(
                    actionButton("addInput","Add Location", icon = icon("plus"), style = "font-size: 8pt;"),
                    actionButton("removeInput","Remove Location", icon = icon("minus"), style = "font-size: 8pt;"),
                    style = "padding-left: 15px;"
                  )
                )
              ),
              column(
                width = 4, 
                div(
                  actionButton("reset", "Reset", icon = icon("undo"), style = "font-size: 8pt;"),
                  style = "text-align: right;"
                )
              )
            ),
            br()
          ),
          div(p()),
          uiOutput("city_list"),
          div(
            br(),
            actionButton("getRoute","Get Optimized Route", icon = icon("calculator"), style = "width: 100%; height: 50px; font-size: 12pt; font-weight: bold;")
          ),
          uiOutput("optimized_link")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          google_mapOutput(outputId = "mapEurope"),
          div(
            htmlOutput("txtOut"),
            style = "font-size: 10pt;"
            )
        )
      )
    )
  )


## server

server <- function(input, output, session) {

  
  ## pop-up modal asking for Google Maps API key
  
  showModal(modalDialog(
    div(
      textInput("api", "Please enter your Google Maps API key."),
      div(tagList(actionButton("ok", "Enter")), style = "text-align: right;")
    ),
    footer =
      div(
        tagList(
          a("Click here", href = "https://mapsplatform.google.com/", target = "_blank"), 
          "to obtain a Google Maps API key."
        ),
        p(),
        div(
          p("WARNING: This tool will only work if a valid key has been entered.  If the map fails to load after entering your key, you will need to refresh the page and enter a valid key when prompted."),
          style = "font-style: italic; color: red;"
        ),
        style = "text-align: left;"
      )
  ))
  
  
  ## once key is entered, hide modal, set key, and load map
  
  observeEvent(input$ok, {
    
    api_set <<- 2
    
    api_key <- input$api
    
    removeModal()
    
    set_key(key = api_key)
    
    output$mapEurope <- renderGoogle_map({
      google_map(
        location = c(37.4221, -122.0841),
        scale_control = TRUE, 
        height = "100%")
    })
    
  })
  
  
  ## show two location inputs as default
  
  ids <- c(1, 2)
  
  output$city_list <- renderUI({
    tagList(
      lapply(1:length(ids),function(i){
        fluidRow(
          column(width = 9, textInput(paste0("txtInput",ids[i]), sprintf("Location #%d",ids[i]))),
          column(width = 3, 
            div(
              br(),
              radioButtons(paste0("constraint",ids[i]), NULL, choices = c("start", "end"), selected = character(0))))
        )
      })
    )
  })
  
  
  ## when reset clicked, clear map, clear inputs, return to two inputs, and hide detailed route link
  
  observeEvent(input$reset,{
    
    google_map_update(map_id = "mapEurope") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers()
    
    ids <- c(1, 2)
    
    output$city_list <- renderUI({
      tagList(
        lapply(1:length(ids),function(i){
          fluidRow(
            column(width = 9, textInput(paste0("txtInput",ids[i]), sprintf("Location #%d",ids[i]))),
            column(width = 3, 
                   div(
                     br(),
                     radioButtons(paste0("constraint",ids[i]), NULL, choices = c("start", "end"), selected = character(0))))
          )
        })
      )
    })
    
    output$txtOut <- renderUI({})
    
    output$optimized_link <- renderUI({})
    
  })
  
  
  ## add another location input unless already showing ten
  
  observeEvent(input$addInput,{
    
    output$txtOut <- renderUI({})
    
    if(length(ids) < 10){
      ids <<- c(ids, max(ids)+1)
    }else{
      output$txtOut <- renderUI({HTML("Cannot add more locations.  Max is ten.")})
    }
    
    output$city_list <- renderUI({
      tagList(
        lapply(1:length(ids),function(i){
          fluidRow(
            column(width = 9, textInput(paste0("txtInput",ids[i]), sprintf("Location #%d",ids[i]))),
            column(width = 3, 
                   div(
                     br(),
                     radioButtons(paste0("constraint",ids[i]), NULL, choices = c("start", "end"), selected = character(0))))
          )
        })
      )
    })
  })
  
  
  ## remove a location input unless only two left
  
  observeEvent(input$removeInput,{
    
    output$txtOut <- renderUI({})
    
    if(length(ids) > 2){
      ids <<- head(ids, -1)
    }else{
      output$txtOut <- renderUI({HTML("Must have at least two locations.")})
    }
    
    output$city_list <- renderUI({
      tagList(
        lapply(1:length(ids),function(i){
          fluidRow(
            column(
              width = 9, 
              textInput(paste0("txtInput",ids[i]), sprintf("Location #%d",ids[i]))
            ),
            column(
              width = 3, 
              div(
                br(),
                radioButtons(paste0("constraint",ids[i]), NULL, choices = c("start", "end"), selected = character(0))
              )
            )
          )
        })
      )
    })
    
    
  })
  
  
  ## when get route clicked, take in inputted locations and return optimized route
  
  observeEvent(input$getRoute,{
    
    
    show_spinner()

    ## create vector of inputted locations and check at least two locations, up to one origin, and up to one destination
    
    inputs <<- c()
    
    for(i in 1:length(ids)){
      if(input[[paste0("txtInput",i)]] != ""){
        inputs <<- c(inputs, input[[paste0("txtInput",i)]])
      }
    }
    
    origin <<- NULL
    
    num_origins <<- 0
    
    distination <<- NULL
    
    num_destinations <<- 0
    
    for(i in 1:length(inputs)){
      if(!is.null(input[[paste0("constraint",i)]])){
        if(input[[paste0("constraint",i)]] == "start"){
          origin <<- i
          num_origins <<- num_origins + 1
        }else if(input[[paste0("constraint",i)]] == "end"){
          destination <<- i
          num_destinations <<- num_destinations + 1
        }
      }
    }
    
    if(length(inputs) < 2){
      output$txtOut <- renderUI({HTML("Must have at least two locations.")})
      hide_spinner()
    }else if(num_origins > 1){
      output$txtOut <- renderUI({HTML("You may only designate one location as your starting location.")})
      hide_spinner()
    }else if(num_destinations > 1){
      output$txtOut <- renderUI({HTML("You may only designate one location as your ending location.")})
      hide_spinner()
    }else{
      
      
      ## figure out optimal route

      coordinates <<- data.frame()
      
      distances <<- matrix(NA, length(inputs), length(inputs))
      
      for(i in 1:length(inputs)){
        for(j in 1:length(inputs)){
          if(i == j){
            distances[i,j] <<- 0
            test <<- google_geocode(inputs[i])
            coordinates[i, "Full name"] <<- test$results$formatted_address
            coordinates[i,"Latitude"] <<- test$results$geometry$location$lat
            coordinates[i,"Longitude"] <<- test$results$geometry$location$lng
          }else{
            test <<- google_directions(origin = inputs[i], destination = inputs[j], simplify = T, mode = "driving", departure_time = "now", traffic_model = "best_guess")
            if(!is.null(test$routes$legs[[1]]$duration_in_traffic$value)){
              distances[i,j] <<- test$routes$legs[[1]]$duration_in_traffic$value
            }
          }
        }
      }
      
      distances <- as.data.frame(distances)
      
      names(distances)[1:length(inputs)] <<- inputs
      
      row.names(distances) <<- inputs
      
      if(any(is.na(distances))){
        output$txtOut <- renderUI({HTML("Driving between specified locations is impossible!")})
        hide_spinner()
      }else{
        
        possibilities <- permn(length(inputs))
        
        if(num_origins == 1){
          x <- list()
          for(i in 1:length(possibilities)){
            if(possibilities[[i]][1] == origin){
              x <- c(x, list(possibilities[[i]]))
            }
          }
          possibilities <- x
        }
        
        if(num_destinations == 1){
          x <- list()
          for(i in 1:length(possibilities)){
            if(tail(possibilities[[i]], 1) == destination){
              x <- c(x, list(possibilities[[i]]))
            }
          }
          possibilities <- x
        }
        
        for(i in 1:length(possibilities)){
          travel_time <- 0
          for(j in 1:(length(possibilities[[i]]) - 1)){
            travel_time <- travel_time + distances[possibilities[[i]][j], possibilities[[i]][j+1]]
          }
          if(i == 1){
            optimized <- possibilities[[i]]
            time <- travel_time
          }
          if(time > travel_time){
            optimized <- possibilities[[i]]
            time <- travel_time
          }
        }
        
        weeks <<- floor(time / 604800)
        
        days <<- floor((time - (weeks * 604800)) / 86400)
        
        hours <<- floor((time - (weeks * 604800) - (days * 86400)) / 3600)
        
        mins <<- floor((time - (weeks * 604800) - (days * 86400) - (hours * 3600)) / 60)
        
        secs <<- time - (weeks * 604800) - (days * 86400) - (hours * 3600) - (mins * 60)
        
        if(weeks == 1){
          week_text <<- paste0(weeks, " week")
        }else if(weeks > 1){
          week_text <<- paste0(weeks, " weeks")
        }
        
        if(days == 1){
          day_text <<- paste0(days, " day")
        }else if(days > 1){
          day_text <<- paste0(days, " days")
        }
        
        if(hours == 1){
          hour_text <<- paste0(hours, " hour")
        }else if(hours > 1){
          hour_text <<- paste0(hours, " hours")
        }
        
        if(mins == 1){
          min_text <<- paste0(mins, " min")
        }else if(mins > 1){
          min_text <<- paste0(mins, " mins")
        }
        
        if(weeks > 0){
          estimate <- paste(week_text, day_text, hour_text, min_text, sep = " ")
        }else if(days > 0){
          estimate <- paste(day_text, hour_text, min_text, sep = " ")
        }else if(hours > 0){
          estimate <- paste(hour_text, min_text, sep = " ")
        }else if(mins > 0){
          estimate <- min_text
        }else{
          if(secs == 1){
            estimate <- paste0(secs, " sec")
          }else{
            estimate <- paste0(secs, " secs")
          }
        }
        
        ## return optimized route
        
        df_way <- data.frame()
        
        for(i in 1:length(optimized)){
          df_way <- 
            rbind(
              df_way, 
              data.frame(
                address = coordinates[as.numeric(optimized[i]),"Full name"], 
                lat = coordinates[as.numeric(optimized[i]),"Latitude"], 
                lon = coordinates[as.numeric(optimized[i]),"Longitude"], 
                path = as.character(i)
              )
            )
          
          optimized[i] <- coordinates[as.numeric(optimized[i]),"Full name"]
        }
        
        route <- ""
        
        link <- "https://www.google.com/maps/dir/"
        
        for(i in 1:length(optimized)){
          if(i == 1){
            start <- optimized[i]
            route <- paste0(optimized[i])
          } else if(i == 2 & i != length(optimized)){
            waypoints <- list(stop = optimized[i])
            route <- paste0(route, " >> ", optimized[i])
          } else if(i == length(optimized)){
            stop <- optimized[i]
            route <- paste0(route, " >> ", optimized[i])
          } else{
            waypoints <- c(waypoints, stop = optimized[i])
            route <- paste0(route, " >> ", optimized[i])
          }
          link <- paste0(link, optimized[i],"/")
        }
        
        if(length(optimized) == 2){
          res <- 
            google_directions(
              origin = start,
              destination = stop,
              optimise_waypoints = FALSE,
              mode = "driving"
            )
        }else{
          res <- 
            google_directions(
              origin = start,
              waypoints = waypoints,
              destination = stop,
              optimise_waypoints = FALSE,
              mode = "driving"
            )
        }
        
        df_route <- data.frame(polyline = res$routes$overview_polyline$points, route = route)
        
        google_map_update(map_id = "mapEurope") %>%
        clear_bounds() %>%
        clear_traffic() %>%
        clear_polylines() %>%
        clear_markers() %>%
        add_polylines(
          data = df_route,
          polyline = "polyline",
          stroke_colour = "#FF33D6",
          stroke_weight = 7,
          stroke_opacity = 0.7,
          info_window = "route",
          load_interval = 100
        ) %>%
        add_markers(
          data = df_way,
          lat = "lat",
          lon = "lon",
          info_window = "address",
          label = "path",
          update_map_view = T
        )
        
        route_text <- paste0("Your optimized route is ", route,".")
        
        estimate_text <- paste0("Estimated driving time is ", estimate, ".")
        
        output$txtOut <- renderUI({HTML(paste(route_text, estimate_text, sep = "<br>"))})
  
        output$optimized_link <- renderUI({
          div(
            p(),
            actionButton("externalLink1", "Detailed directions for your optimized route.", onclick = paste0("window.open('", link,"', '_blank')"), style = "width: 100%; font-size: 8pt; font-weight: bold; background-color: #4472C4; color: white; border-color: black;")
          )
        })
        
        hide_spinner()
        
      }
    }
  })
}


## app

shinyApp(ui, server)
