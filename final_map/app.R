#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(countrycode)
library(knitr)
library(stringr)
library(rsconnect)
library(maps)
library(dplyr)
source("facility.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Number of Mental Health Facilities in Each State"),
    sidebarLayout(
        sidebarPanel(
            selectInput("facilities",
                        label = h3("Type of Facility"),
                                   c("Total" = "total",
                                     "Psychiatric Hospital" = "psych",
                                     "Separate inpatient psychiatric unit of a general hospital" = "separate",
                                     "Residential treatment center for children" = "res_child",
                                     "Residential treatment center for adults" = "res_adult",
                                     "Other type of residential treatment facility" = "other_res",
                                     "Veterans Administration medical center (VAMC)" = "vet",
                                     "Community mental health center (CMHC)" = "com",
                                     "Partial hospitalization/day treatment facility" = "partial",
                                     "Outpatient mental health facility" = "outpatient",
                                     "Multi-setting mental health facility" = "multi",
                                     "Other" = "other")),
           selectInput(inputId = "state",
                        label = h3("Select a State"),
                        c("Alabama (AL)" = "AL",
                        "Arizona (AZ)" = "AZ", "Arkansas (AR)" = "AR",
                        "California (CA)" = "CA", "Colorado (CO)" = "CO",
                        "Connecticut (CT)" = "CT", "Delaware (DE)" = "DE",
                        "Florida (FL)" = "FL", "Georgia (GA)" = "GA",
                        "Idaho (ID)" = "ID",
                        "Illinois (IL)" = "IL", "Indiana (IN)" = "IN",
                        "Iowa (IA)" = "IA", "Kansas (KS)" = "KS",
                        "Kentucky (KY)" = "KY", "Louisiana (LA)" = "LA",
                        "Maine (ME)" = "ME", "Maryland (MD)" = "MD",
                        "Massachusetts (MA)" = "MA", "Michigan (MI)" = "MI",
                        "Minnesota (MN)" = "MN", "Mississippi (MS)" = "MS",
                        "Missouri (MO)" = "MO", "Montana (MT)" = "MT",
                        "Nebraska (NE)" = "NE", "Nevada (NV)" = "NV",
                        "New Hampshire (NH)" = "NH", "New Jersey (NJ)" = "NJ",
                        "New Mexico (NM)" = "NM", "New York (NY)" = "NY",
                        "North Carolina (NC)" = "NC",
                         "North Dakota (ND)" = "ND",
                        "Ohio (OH)" = "OH", "Oklahoma (OK)" = "OK",
                        "Oregon (OR)" = "OR", "Pennsylvania (PA)" = "PA",
                        "Rhode Island (RI)" = "RI",
                         "South Carolina (SC)" = "SC",
                        "South Dakota (SD)" = "SD", "Tennessee (TN)" = "TN",
                        "Texas (TX)" = "TX", "Utah (UT)" = "UT",
                        "Vermont (VT)" = "VT", "Virginia (VA)" = "VA",
                        "Washington (WA)" = "WA", "West Virginia (WV)" = "WV",
                       "Wisconsin (WI)" = "WI", "Wyoming (WY)" = "WY"
                        ))), 
        mainPanel(
            plotlyOutput("facility_map"),
            plotlyOutput("facility_graph")
    ),
),
)
server <- function(input, output) {
    facilities_input <- reactive({
        if (input$facilities == "total") {
            dataset <- map_total
        }
        if (input$facilities == "psych") {
            dataset <- map_psych
        }
        if (input$facilities == "separate") {
            dataset <- map_separate
        }
        if (input$facilities == "res_child") {
            dataset <- map_res_child
        }
        if (input$facilities == "res_adult") {
            dataset <- map_res_adult
        }
        if (input$facilities == "other_res") {
            dataset <- map_other_res
        }
        if (input$facilities == "vet") {
            dataset <- map_vet
        }
        if (input$facilities == "com") {
            dataset <- map_com
        }
        if (input$facilities == "partial") {
            dataset <- map_partial
        }
        if (input$facilities == "outpatient") {
            dataset <- map_outpatient
        }
        if (input$facilities == "multi") {
            dataset <- map_multi
        }
        else if (input$facilities == "other") {
            dataset <- map_other
        }
        return(dataset)
    })
    output$facility_map <- renderPlotly({
        {facilities_input() %>%
                ggplot() +
                geom_polygon(
                    mapping = aes(x = long, y = lat, group = group, 
                                  LST = LST, fill = number,
                                  text = paste("State:", LST,
                                               "<br>Number of Mental Health Facilities:", number)),
                    color = "gray", size = 0.3
                ) +
                coord_map() +
                scale_fill_continuous(limits = c(0, max(facilities_input()$number)),
                                      na.value = "white", low = "slateblue4", high = "turquoise") +
                labs(title = "Number of Mental Health Facilities in Each State, 2018") +
                labs(fill = "Number of Facilities") +
                blank_theme
    } %>%
            ggplotly(tooltip = "text")
        })
    output$facility_graph <- renderPlotly({
        {facilities_input() %>%
                full_join(age_data, by = "CASEID") %>%
                mutate(LST = LST.x) %>%
                filter(LST == input$state) %>%
                select(CASEID, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS, long, lat, group, order, number, LST) %>%
                na.omit() %>%
                distinct(CASEID, .keep_all = TRUE) %>%
                select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
                summarise_each(funs = sum) %>%
                gather() %>%
                ggplot() +
                geom_col(aes(x = key, y = value, fill = value,
                             text = paste("Number of Facilities:", value)))
        } %>%
            ggplotly(tooltip = "text")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
