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
                                   c("Total" = "Total Mental Health Facilities",
                                     "Psychiatric Hospital" = "Psychiatric Hospitals",
                                     "Separate inpatient psychiatric unit of a general hospital" = "Separate Inpatient Psychiatric Units of a General Hospital",
                                     "Residential treatment center for children" = "Residential Treatment Centers for Children",
                                     "Residential treatment center for adults" = "Residential Treatment Centers for Adults",
                                     "Other type of residential treatment facility" = "Other Types of Residential Facilities",
                                     "Veterans Administration medical center (VAMC)" = "Veterans Administration Medical Centers",
                                     "Community mental health center (CMHC)" = "Community Mental Health Centers",
                                     "Partial hospitalization/day treatment facility" = "Partial Hospitalization/Day Treatment Facilities",
                                     "Outpatient mental health facility" = "Outpatient Mental Health Facilities",
                                     "Multi-setting mental health facility" = "Multi-setting Mental Health Facilities",
                                     "Other" = "Other")),
           selectInput(inputId = "state",
                        label = h3("Select a State"),
                        c("Alabama (AL)" = "AL", "Alaska (AK)" = "AK",
                        "Arizona (AZ)" = "AZ", "Arkansas (AR)" = "AR",
                        "California (CA)" = "CA", "Colorado (CO)" = "CO",
                        "Connecticut (CT)" = "CT", "Delaware (DE)" = "DE",
                        "Florida (FL)" = "FL", "Georgia (GA)" = "GA",
                        "Hawaii (HI)" = "HI", "Idaho (ID)" = "ID",
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
        if (input$facilities == "Total Mental Health Facilities") {
            dataset <- final_total
        }
        if (input$facilities == "Psychiatric Hospitals") {
            dataset <- final_psych
        }
        if (input$facilities == "Separate Inpatient Psychiatric Units of a General Hospital") {
            dataset <- final_separate
        }
        if (input$facilities == "Residential Treatment Centers for Children") {
            dataset <- final_res_child
        }
        if (input$facilities == "Residential Treatment Centers for Adults") {
            dataset <- final_res_adult
        }
        if (input$facilities == "Other Types of Residential Facilities") {
            dataset <- final_other_res
        }
        if (input$facilities == "Veterans Administration Medical Centers") {
            dataset <- final_vet
        }
        if (input$facilities == "Community Mental Health Centers") {
            dataset <- final_com
        }
        if (input$facilities == "Partial Hospitalization/Day Treatment Facilities") {
            dataset <- final_partial
        }
        if (input$facilities == "Outpatient Mental Health Facilities") {
            dataset <- final_outpatient
        }
        if (input$facilities == "Multi-setting Mental Health Facilities") {
            dataset <- final_multi
        }
        else if (input$facilities == "Other") {
            dataset <- final_other
        }
        return(dataset)
    })
    output$facility_map <- renderPlotly({
        {facilities_input() %>%
                full_join(state_map, by = "LST") %>%
                select(CASEID, long, lat, group, order, LST, number) %>%
                distinct(order, .keep_all = TRUE) %>%
                ggplot() +
                geom_polygon(
                    mapping = aes(x = long, y = lat, group = group, 
                                  LST = LST, fill = number,
                                  text = paste("State:", LST,
                                               "<br>Number of Facilities:", number)),
                    color = "gray", size = 0.3
                ) +
                coord_map() +
                scale_fill_continuous(limits = c(0, max(facilities_input()$number)),
                                      na.value = "white", low = "slateblue4", high = "turquoise") +
                labs(title = str_wrap(paste("Number of", input$facilities, "in Each State, 2018"), 60)) +
                labs(fill = "Number of Facilities") +
                blank_theme
    } %>%
            ggplotly(tooltip = "text")
        })
    output$facility_graph <- renderPlotly({
        level_order <- c('CHILDAD', 'ADOLES', 'YOUNGADULTS', 'ADULT', 'SENIORS')
        {facilities_input() %>%
                left_join(age_data, by = "CASEID") %>%
                filter(LST.x == input$state) %>%
                select(CASEID, CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS, LST.x) %>%
                na.omit() %>%
                distinct(CASEID, .keep_all = TRUE) %>%
                select(CHILDAD, ADOLES, YOUNGADULTS, ADULT, SENIORS) %>%
                summarise_each(funs = sum) %>%
                gather() %>%
                ggplot() +
                geom_col(aes(x = factor(key, level = level_order), y = value, fill = value,
                             text = paste("Number of Facilities:", value))) +
                scale_x_discrete(labels=c("0-12", "13-17", "18-25", "26-64", "65+")) +
                labs(title = str_wrap(paste("Number of", input$facilities,
                                            "in", input$state,", 2018"), 60)) +
                xlab("Ages Accepted") + 
                ylab("Number of Facilities") +
                scale_fill_continuous(name = "Number of Facilities")
            } %>%
            ggplotly(tooltip = "text")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
