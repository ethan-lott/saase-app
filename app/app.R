# Load packages, data, and global variables
source("global.R")

# Implement Shiny UI
ui <- fluidPage(
    titlePanel("SAASE Orientation Survey: Explore the Responses"),
    tabsetPanel(
        tabPanel("Free Exploration",
            sidebarLayout(
                # Sidebar for filtering data.
                sidebarPanel(
                    style = "min-height: 85vh;",
                    # Select topic
                    selectInput(
                        inputId = "free_q",
                        label = "Question:",
                        choices = names(ors_kws)
                    ),
                    # Select demographic to filter
                    selectInput(
                        inputId = "filt",
                        label = "Filter by:",
                        choices = c(
                            "--",
                            "Population" = "STUDENT_POPULATION_DESC",
                            "First Generation" = "FIRST_GENERATION_IND",
                            "Housing Type" = "HOUSING_TYPE",
                            "Living Community" = "COMMUNITY",
                            "College" = "COLLEGE_DESC"
                        )
                    ),
                    # Filter options
                    uiOutput("filt_val_ui"),
                    h5(HTML("<strong>Data Remaining:</strong>")),
                    uiOutput("curr_filts", style="font-size: 12px;"),
                    # Button to update the charts.
                    actionButton(
                        inputId = "free_load",
                        label = "Load Chart"
                    )
                ),
                mainPanel(
                    column(12, align="center",
                        h4(textOutput("free_title")),
                        plotOutput("free_pie", height=600)
                    )
                )
            )
        ),
        tabPanel("Demographic Analysis",
            sidebarLayout(
                sidebarPanel(
                    style = "height: 85vh;",
                    # Select topic
                    selectInput(
                        inputId = "stat_topic",
                        label = "Topic:",
                        choices = c("Sociality", "Wellness", "Success", "Grit")
                    ),
                    # Select demographic to filter
                    selectInput(
                        inputId = "stat_demo",
                        label = "Demographic:",
                        choices = c(
                            "Population" = "STUDENT_POPULATION_DESC",
                            "First Generation" = "FIRST_GENERATION_IND",
                            "Housing Type" = "HOUSING_TYPE",
                            "Living Community" = "COMMUNITY",
                            "College" = "COLLEGE_DESC"
                        ),
                    ),
                    # Button to update the charts.
                    actionButton(
                        inputId = "stat_load",
                        label = "Load Analysis"
                    )
                ),
                mainPanel(
                    style="min-height: 80vh;",
                    column(6, align="center",
                        h4(textOutput("demo_title")),
                        plotOutput("demo_plot", height="70vh")
                    ),
                    column(6, align="center",
                        h4("Statistical Significance"),
                        verbatimTextOutput("test_results")
                    )
                )
            )
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
    
    # Render demographics plot
    stat_topic <- eventReactive(input$stat_load, input$stat_topic)
    stat_demo <- eventReactive(input$stat_load, input$stat_demo)
    output$demo_plot <- renderPlot({
        stat_qs <- topic_qs[[stat_topic()]]
        df_summary <- ors_pde_num |>
            select(all_of(stat_qs), stat_demo()) |>
            group_by(get(stat_demo())) |>
            summarise(
                avg = mean(rowMeans(across(all_of(stat_qs)), na.rm = TRUE)),
                sd = sd(rowMeans(across(all_of(stat_qs)), na.rm = TRUE)),  # Standard deviation
                n = n()
            ) |>
            mutate(se = sd / sqrt(n))
        
        colnames(df_summary)[1] <- stat_demo()
        output$demo_title = renderText({paste("Average ",stat_topic()," Responses")})
        ggplot(df_summary, aes(x = .data[[stat_demo()]], y = avg)) +
            geom_point(size = 3, color = "blue") +  # Plot the averages
            geom_errorbar(aes(ymin = avg - se, ymax = avg + se), width = 0.2, color = "black") +  # Error bars
            labs(x = NULL, y = NULL) +
            theme_bw() +
            scale_y_continuous(
                limits = c(3, 5),
                breaks = c(3, 4, 5),
                labels = c("Neither agree\nnor disagree", "Somewhat\nagree", "Strongly\nagree")
            ) +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
                axis.text.y = element_text(angle = 45, hjust = 0.5, size = 10)
            )
        
    })
    
    # Update filtering options based on selected filter group.
    output$filt_val_ui <- renderUI({
        filt <- input$filt
        if (filt != "--") {
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
        if (filt != "None") {selected_filts[[filt]] <- input$filt_vals}
    })
    
    q <- eventReactive(input$free_load, input$free_q)
    
    # Update data frame used for plotting when needed.
    filtered_data <- eventReactive(input$free_load, {
        df_filt <- ors_pde_raw |>
            select(all_of(unname(ors_kws[q()])), names(selected_filts)) |>
            filter(
                STUDENT_POPULATION_DESC %in% selected_filts[["STUDENT_POPULATION_DESC"]],
                FIRST_GENERATION_IND %in% selected_filts[["FIRST_GENERATION_IND"]],
                HOUSING_TYPE %in% selected_filts[["HOUSING_TYPE"]],
                COMMUNITY %in% selected_filts[["COMMUNITY"]],
                COLLEGE_DESC %in% selected_filts[["COLLEGE_DESC"]]
            )
        df_filt
    })
    
    # Render pie chart for a single question
    output$free_pie <- renderPlot({
        qCol <- unname(ors_kws[q()])
        output$free_title <- renderText({ors_qs[qCol]})
        df_filt <- filtered_data() |>
            select(all_of(qCol)) |>
            mutate(Response = factor(get(qCol), levels = if (substr(qCol,1,1) != "Q") grit_response_order else q_response_order)) |>
            count(Response, .drop = FALSE)
        ggplot(df_filt, aes(x = "", y = n, fill = Response)) + 
            geom_bar(stat = "identity") + 
            coord_polar("y", start = 0) +
            scale_fill_brewer(
                palette = "RdYlGn",
                direction = -1
            ) +
            labs(x = "", y = "") +
            theme_void() 
    })
    
    # Display data currently allowed by filters
    output$curr_filts <- renderUI({
        txt <- ""
        for (name in names(selected_filts)) {
            txt <- paste(txt, "<strong>",name,"</strong>", ":", paste(selected_filts[[name]], collapse=", "), "<br>")
        }
        HTML(paste(txt,"<br>"))
    })

    # ANOVA Test Results
    output$test_results <- renderText({
        stat_qs <- topic_qs[[stat_topic()]]
        
        df <- ors_pde_num |>
            select(all_of(stat_qs), stat_demo()) |>
            mutate(avg = rowMeans(across(all_of(stat_qs))))
        
        formula <- as.formula(paste("avg ~", stat_demo()))
        res <- summary(aov(formula, data = df), )
        capture.output(print(res)) |> paste(collapse = "\n")
    })
}

# Run the application 
shinyApp(ui, server)
