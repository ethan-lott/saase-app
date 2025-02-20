#    https://shiny.posit.co/

library(shiny)
library(ggplot2)
library(tidyverse)
library(readxl)
library(patchwork)

# Import data and join tables.
ors_raw <- read_excel("data/or_survey_sim_final_2023-11-29.xlsx")
pde_raw <- read_excel("data/pde_sim_final_2023-11-29.xlsx")
ors_pde_raw <- merge(ors_raw, pde_raw)

# Map text responses to ordinal data.
response_map <- c(
    "Strongly agree" = 1,
    "Somewhat agree" = 2,
    "Neither agree nor disagree" = 3,
    "Somewhat disagree" = 4,
    "Strongly disagree" = 5,
    "Very much like me" = 1,
    "Mostly like me" = 2,
    "Somewhat like me" = 3,
    "Not much like me" = 4,
    "Not at all like me" = 5
)
ors_num <- ors_raw
ors_num[-1] <- ors_num[-1] |>
    mutate(across(everything(), ~ recode(.x, !!!response_map)))
ors_pde_num <- merge(ors_num, pde_raw)

# Full statement of each survey question.
ors_qs <- c(
    "Q4_1" = "I feel I belong at Binghamton University.",
    "Q4_2" = "I am ready to meet the academic challenges of Binghamton University.",
    "Q11_1" = "Binghamton University is concerned with my personal health/wellness.",
    "Q11_2" = "Binghamton University has a number of services available to help me be healthy at college.",
    "Q15_1" = "I feel like I have the resources to make me a financially responsible college student.",
    "Q15_2" = "I feel connected to the Binghamton University community.",
    "Q15_3" = "Binghamton University is a place where I am able to perform up to my full potential.",
    "Q15_4" = "I have found one or more communities or groups where I feel I belong at Binghamton University.",
    "Q15_5" = "I am ready to make friends at Binghamton University.",
    "Grit_1" = "New ideas and projects sometimes distract me from previous ones.",
    "Grit_2" = "Setbacks don’t discourage me.",
    "Grit_3" = "I have been obsessed with a certain idea or project for a short time but later lost interest.",
    "Grit_4" = "I am a hard worker.",
    "Grit_5" = "I often set a goal but later choose to pursue a different one.",
    "Grit_6" = "I have difficulty maintaining my focus on projects that take more than a few months to complete.",
    "Grit_7" = "I finish whatever I begin.",
    "Grit_8" = "I am diligent."
)

q_response_order <- c(
    "Strongly agree",
    "Somewhat agree",
    "Neither agree nor disagree",
    "Somewhat disagree",
    "Strongly disagree"
)

grit_response_order <- c(
    "Very much like me",
    "Mostly like me",
    "Somewhat like me",
    "Not much like me",
    "Not at all like me"
)

# Survey questions associated with each topic.
topic_qs <- list(
    "Sociality" = c("Q4_1", "Q15_2", "Q15_4", "Q15_5"),
    "Wellness" = c("Q11_1", "Q11_2"),
    "Success" = c("Q4_2", "Q15_1", "Q15_3"),
    "Grit" = c("Grit_1", "Grit_2", "Grit_3", "Grit_4", "Grit_5", "Grit_6", "Grit_7", "Grit_8")
)

# Implement Shiny UI
ui <- fluidPage(
    titlePanel("SAASE Orientation Survey: Explore the Data"),
    fluidRow(
        sidebarLayout(
            # Sidebar for filtering data.
            sidebarPanel(
                style = "min-height: 500px;",
                # Select topic
                selectInput(
                    inputId = "topic",
                    label = "Topic:",
                    choices = c("Sociality", "Wellness", "Success", "Grit")
                ),
                
                # Select demographic to filter
                selectInput(
                    inputId = "filt",
                    label = "Filter by:",
                    choices = c(
                        "None",
                        "Population" = "STUDENT_POPULATION_DESC",
                        "First Generation" = "FIRST_GENERATION_IND",
                        "Housing Type" = "HOUSING_TYPE",
                        "Living Community" = "COMMUNITY",
                        "College" = "COLLEGE_DESC"
                    )
                ),
                
                # Filter options
                uiOutput("filt_val_ui"),
                verbatimTextOutput("curr_filts"),
                
                # Button to update the charts.
                actionButton(
                    inputId = "topic_load",
                    label = "Load Chart"
                )
            ),
            mainPanel(
                plotOutput("full_pie", height=500)
            )
        )
    ),
    
    # Display charts for each relevant question.
    fluidRow(
        column(12, align="center",
               h3("Individual Questions"),
               plotOutput("patched_pies", width="100%")
        )
    )
)

# Define Shiny server logic.
server <- function(input, output) {
    
    # Keep track of which filters are active.
    selected_filts <- reactiveValues(
        STUDENT_POPULATION_DESC = unique(ors_pde_raw$STUDENT_POPULATION_DESC),
        FIRST_GENERATION_IND = unique(ors_pde_raw$FIRST_GENERATION_IND),
        HOUSING_TYPE = unique(ors_pde_raw$HOUSING_TYPE),
        COMMUNITY = unique(ors_pde_raw$COMMUNITY),
        COLLEGE_DESC = unique(ors_pde_raw$COLLEGE_DESC)
    )
    
    # Update filtering options based on selected filter group.
    output$filt_val_ui <- renderUI({
        filt <- input$filt
        if (filt != "None") {
            col <- get(filt, envir = as.environment(ors_pde_raw))
            checkboxGroupInput(
                inputId = "filt_vals",
                label = "",
                choices = unique(col),
                selected = selected_filts[[filt]]
            )
        } else {
            NULL
        }
    })
    
    # Update selected_filters whenever filt_vals changes
    observeEvent(input$filt_vals, {
        filt <- input$filt
        if (filt != "None") {
            selected_filts[[filt]] <- input$filt_vals
        }
    })
    
    # Update data frame used for plotting when needed.
    filtered_data <- eventReactive(input$topic_load, {
        topic <- input$topic
        qs <- topic_qs[[topic]]  # Get questions for the selected topic
        
        df_filt <- ors_pde_raw |>
            select(all_of(qs), names(selected_filts)) |>
            filter(
                STUDENT_POPULATION_DESC %in% selected_filts[["STUDENT_POPULATION_DESC"]],
                FIRST_GENERATION_IND %in% selected_filts[["FIRST_GENERATION_IND"]],
                HOUSING_TYPE %in% selected_filts[["HOUSING_TYPE"]],
                COMMUNITY %in% selected_filts[["COMMUNITY"]],
                COLLEGE_DESC %in% selected_filts[["COLLEGE_DESC"]]
            )
            
        df_filt  # This is the dataset that will be used when `topic_load` is clicked
    })
    
    # Construct total pie chart.
    output$full_pie <- renderPlot({
        req(input$topic_load)  # Ensure button press is required
        
        # Get selected topic and questions
        topic <- isolate(input$topic)  # Avoid reactive triggering on every change
        qs <- topic_qs[[topic]]
        
        # Filter dataset based on current selections
        df_filt <- filtered_data() |>
            select(all_of(qs)) |>
            pivot_longer(cols = everything(), names_to = "Question", values_to = "Response") |>
            count(Response)
        
        # Define response levels based on topic type
        x_vals <- if (topic == "Grit") grit_response_order else q_response_order
        
        # Ensure all possible responses are included, even if count is 0
        df_filt <- df_filt |>
            complete(Response = x_vals, fill = list(n = 0))  # Fill missing categories with 0
        
        # Create the pie chart
        ggplot(df_filt, aes(x = "", y = n, fill = factor(Response, levels = x_vals))) + 
            geom_bar(stat = "identity") +
            coord_polar("y", start = 0) +
            scale_fill_brewer(palette = "RdYlGn", direction = -1) +
            labs(title = paste("Overall Responses for", topic),
                 fill = "Response",
                 x = "", y = "") +
            theme(
                panel.grid = element_blank(),  # Remove grid lines
                panel.border = element_blank(),  # Remove panel border
                axis.text = element_blank(),  # Remove axis text
                axis.ticks = element_blank()  # Remove axis ticks
            )
    })
    
    # Construct patchwork of individual pie charts.
    output$patched_pies <- renderPlot({
        req(input$topic_load)
        
        df_filt <- filtered_data()
        topic <- isolate(input$topic) 
        qs <- topic_qs[[topic]]
        ps <- NULL
        for (q in qs) {
            df <- df_filt |>
                mutate(response = factor(get(q), levels = if (topic == "Grit") grit_response_order else q_response_order)) |>
                count(response, .drop = FALSE)  # Ensures missing values appear with count = 0
            
            p_i <- ggplot(df, aes(x = "", y = n, fill = response)) + 
                geom_bar(stat = "identity") + 
                coord_polar("y", start = 0) +
                scale_fill_brewer(
                    labels = if (topic == "Grit") grit_response_order else q_response_order,
                    palette = "RdYlGn",
                    direction = -1
                ) +
                labs(
                    title = paste(strwrap(ors_qs[q], width = if (topic == "Grit") 20 else 50), collapse = "\n"),
                    fill = "Response",
                    x = "", y = ""
                ) +
                theme_void()
            
            ps <- if (is.null(ps)) p_i else ps | p_i
        }
        ps + plot_layout(guides = "collect")
    })
    
    output$curr_filts <- renderText({
        filt <- eventReactive(input$filt_vals, input$filt)
        txt <- "Data Included: \n\n"
        for (name in names(selected_filts)) {
            txt <- paste(txt, name, ":", paste(selected_filts[[name]], collapse=", "), "\n")
        }
        txt
    })

}

# Run the application 
shinyApp(ui, server)
