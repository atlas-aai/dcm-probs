#' Run the LCDM Item Response Function App
#'
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A shiny app.
#' @export
dcmApp <- function(...) {
  sysfonts::font_add_google("Open Sans", "Open Sans")
  
  ui <- dashboardPage(
    title = "DCM Item Response Functions",
    ## header -----
    dashboardHeader(title = logo_measr_light, titleWidth = 400),
    
    ## sidebar -----
    dashboardSidebar(collapsed = TRUE, width = 200,
                     sidebarMenu(menuItem("Github", icon = icon("github"),
                                          href = "https://github.com/wjakethompson/dcm-probs"))),
    
    ## body -----
    dashboardBody(theme_measr_shiny,
                  fluidRow(
                    box(title = "Log-linear Cognitive Diagnostic Model Response Probabilities",
                        width = 12, solidHeader = TRUE, status = "primary",
                        sidebarLayout(
                          sidebarPanel(width = 4,
                            selectInput("atts", "Attributes",
                                        choices = c("One", "Two", "Three"),
                                        selected = "Two"
                            ),
                            p(HTML("<b>Parameter Values (Log-Odds)</b>")),
                            parameter_tabs,
                          ),
                          mainPanel(width = 8,
                            plotOutput("item_response")
                          )
                        )))
                  )
  )
  
  server <- function(input, output, session) {
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
  }
  
  shinyApp(ui, server, ...)
}
