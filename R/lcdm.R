lcdmUI <- function(id) {
  lcdm_parameter_tabs <- tabsetPanel(
    id = NS(id, "params"),
    type = "hidden",
    tabPanel("One",
             numericInput(NS(id, "l1_0"), HTML("Intercept (&lambda;<sub>i,0</sub>)"),
                          value = -1.73, step = 0.1),
             numericInput(NS(id, "l1_11"), HTML("Main effect (&lambda;<sub>i,1(1)</sub>)"),
                          min = 0, value = 3.93, step = 0.1)
    ),
    tabPanel("Two", 
             numericInput(NS(id, "l2_0"), HTML("Intercept (&lambda;<sub>i,0</sub>)"), value = -1.73),
             numericInput(NS(id, "l2_11"), HTML("Main effect for Attribute 1 (&lambda;<sub>i,1(1)</sub>)"),
                          min = 0, value = 2.14, step = 0.1),
             numericInput(NS(id, "l2_12"), HTML("Main effect for Attribute 2 (&lambda;<sub>i,1(2)</sub>)"),
                          min = 0, value = 1.32, step = 0.1),
             numericInput(NS(id, "l2_212"), HTML("Two-way Interaction (&lambda;<sub>i,2(1,2)</sub>)"),
                          min = -1.32, value = 0.47, step = 0.1)
    ),
    tabPanel("Three",
             numericInput(NS(id, "l3_0"), HTML("Intercept (&lambda;<sub>i,0</sub>)"), value = -1.73),
             numericInput(NS(id, "l3_11"), HTML("Main effect for Attribute 1 (&lambda;<sub>i,1(1)</sub>)"),
                          min = 0, value = 0.8, step = 0.1),
             numericInput(NS(id, "l3_12"), HTML("Main effect for Attribute 2 (&lambda;<sub>i,1(2)</sub>)"),
                          min = 0, value = 0.6, step = 0.1),
             numericInput(NS(id, "l3_13"), HTML("Main effect for Attribute 3 (&lambda;<sub>i,1(3)</sub>)"),
                          min = 0, value = 0.9, step = 0.1),
             numericInput(NS(id, "l3_212"), HTML("Two-way Interaction for Attributes 1 and 2 (&lambda;<sub>i,2(1,2)</sub>)"),
                          min = -0.6, value = -0.1, step = 0.1),
             numericInput(NS(id, "l3_213"), HTML("Two-way Interaction for Attributes 1 and 3 (&lambda;<sub>i,2(1,3)</sub>)"),
                          min = -0.8, value = 0.8, step = 0.1),
             numericInput(NS(id, "l3_223"), HTML("Two-way Interaction for Attributes 2 and 3 (&lambda;<sub>i,2(2,3)</sub>)"),
                          min = -0.6, value = 0.9, step = 0.1),
             numericInput(NS(id, "l3_3123"), HTML("Three-way Interaction (&lambda;<sub>i,3(1,2,3)</sub>)"),
                          min = -1.4, value = 0.2, step = 0.1)
    )
  )
  
  box(title = "Log-linear Cognitive Diagnostic Model (LCDM) Response Probabilities",
      width = 12, solidHeader = TRUE, status = "primary",
      sidebarLayout(
        sidebarPanel(width = 4,
                     selectInput(NS(id, "atts"), "Attributes Measured by the Item",
                                 choices = c("One", "Two", "Three"),
                                 selected = "Two"),
                     p(HTML("<b>Parameter Values (Log-Odds)</b>")),
                     lcdm_parameter_tabs),
        mainPanel(width = 8, plotOutput(NS(id, "item_response")))
      ))
}

lcdmServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$atts, {
      updateTabsetPanel(inputId = "params", selected = input$atts)
    })
    
    # interaction constraints
    observeEvent(list(input$l2_11, input$l2_12), {
      updateNumericInput(inputId = "l2_212", min = -1 * min(c(input$l2_11, input$l2_12)))
    })
    observeEvent(list(input$l3_11, input$l3_12), {
      updateNumericInput(inputId = "l3_212", min = -1 * min(c(input$l3_11, input$l3_12)))
    })
    observeEvent(list(input$l3_11, input$l3_13), {
      updateNumericInput(inputId = "l3_213", min = -1 * min(c(input$l3_11, input$l3_13)))
    })
    observeEvent(list(input$l3_12, input$l3_13), {
      updateNumericInput(inputId = "l3_223", min = -1 * min(c(input$l3_12, input$l3_13)))
    })
    observeEvent(list(input$l3_212, input$l3_213, input$l3_223), {
      updateNumericInput(inputId = "l3_3123", min = -1 * min(c(input$l3_11 + input$l3_212 + input$l3_213,
                                                               input$l3_12 + input$l3_212 + input$l3_223,
                                                               input$l3_13 + input$l3_213 + input$l3_223)))
    })
    
    probs <- reactive({
      num_att <- switch(input$atts,
                        One = 1L,
                        Two = 2L,
                        Three = 3L)
      
      all_input <- reactiveValuesToList(input)
      params <- tibble::as_tibble(all_input[grepl(paste0("l", num_att),
                                                  names(all_input))]) |> 
        tidyr::pivot_longer(dplyr::everything()) |> 
        tidyr::separate_wider_regex("name",
                                    c("l[0-9]_", level = "[0-9]",
                                      attributes = "[0-9]*")) |> 
        dplyr::rowwise() |> 
        dplyr::mutate(
          param = dplyr::case_when(
            .data$level == "0" ~ "(Intercept)",
            .data$level == "1" ~ paste0("att",.data$attributes),
            TRUE ~ paste("att", strsplit(.data$attributes, "")[[1]],
                         sep = "", collapse = ":"))
        ) |> 
        dplyr::ungroup() |> 
        dplyr::select("param", param_value = "value")
      
      profiles <- create_profiles(num_att) |> 
        tibble::rowid_to_column(var = "class_id") |>
        tidyr::pivot_longer(cols = -"class_id") |>
        dplyr::summarize(
          label = paste0("[", paste(.data$value, collapse = ","), "]"),
          .by = "class_id"
        ) |>
        dplyr::arrange("class_id") |> 
        dplyr::mutate(label = forcats::fct_inorder(.data$label))
      
      stats::model.matrix(stats::as.formula(paste0("~ .^", max(num_att, 2L))),
                          create_profiles(num_att)) |> 
        tibble::as_tibble() |> 
        tibble::rowid_to_column(var = "class") |> 
        tidyr::pivot_longer(-"class") |> 
        dplyr::filter(.data$value == 1L) |> 
        dplyr::left_join(params, by = c("name" = "param")) |> 
        dplyr::summarize(log_odds = sum(.data$param_value), .by = "class") |> 
        dplyr::mutate(prob = exp(.data$log_odds) / (exp(.data$log_odds) + 1)) |> 
        dplyr::left_join(profiles, by = c("class" = "class_id"))
    })
    output$item_response <- renderPlot({
      showtext::showtext_begin()
      probs() |> 
        dplyr::mutate(
          col = dplyr::case_when(.data$prob == max(.data$prob) ~ "max",
                                 .data$prob == min(.data$prob) ~ "min",
                                 TRUE ~ "mid")
        ) |> 
        ggplot(aes(x = .data$label, y = .data$prob)) +
        geom_col(fill = ggmeasr::palette_measr[4], show.legend = FALSE) +
        expand_limits(y = c(0, 1)) +
        labs(x = "Profile", y = "Probability of Correct Response") +
        ggmeasr::theme_measr() -> p
      print(p)
      showtext::showtext_end()
    }, res = 96)
  })
}
