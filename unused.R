#unused code

# Construct total pie chart.
output$full_pie <- renderPlot({
    req(input$topic_load)  # Ensure button press is required
    # Get selected topic and questions
    topic <- isolate(input$topic)  # Avoid reactive triggering on every change
    qs <- topic_qs[[topic]]
    
    output$title <- renderText({paste("Total Responses for ", topic)})
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
        labs(fill = "Response",
             x = "", y = "") +
        theme_void()
})

# Construct patchwork of individual pie charts.
output$patched_pies <- renderPlot({
    req(input$topic_load)
    
    df_filt <- filtered_data()
    topic <- isolate(input$topic)
    output$ind_title <- renderText({paste("Individual ", topic, " Questions")})
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
                #fill = "Response",
                x = "", y = ""
            ) +
            theme_void() +
            theme(legend.position="none")
        
        ps <- if (is.null(ps)) p_i else ps | p_i
    }
    ps + plot_layout(guides = "collect")
})