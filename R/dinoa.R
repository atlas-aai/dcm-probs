dinoaUI <- function(id, name) {
  box(title = name,
      width = 12, solidHeader = TRUE, status = "primary",
      sidebarLayout(
        sidebarPanel(width = 4,
                     selectInput(NS(id, "atts"), "Attributes Measured by the Item",
                                 choices = c("One", "Two", "Three"),
                                 selected = "Two"),
                     p(HTML("<b>Parameter Values</b>")),
                     numericInput(NS(id, "guess"), HTML("Guessing (<em>g</em><sub>i</sub>)"),
                                  value = 0.1, step = 0.1, min = 0, max = 1),
                     numericInput(NS(id, "slip"), HTML("Slipping (<em>s</em><sub>i</sub>)"),
                                  value = 0.1, step = 0.1, min = 0, max = 1)),
        mainPanel(width = 8, plotOutput(NS(id, "item_response")))
      ))
}

dinaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    probs <- reactive({
      num_att <- switch(input$atts,
                        One = 1L,
                        Two = 2L,
                        Three = 3L)
      
      profiles <- create_profiles(num_att) |> 
        tibble::rowid_to_column(var = "class_id") |>
        tidyr::pivot_longer(cols = -"class_id") |>
        dplyr::mutate(
          label = paste0("[", paste(.data$value, collapse = ","), "]"),
          .by = "class_id"
        ) |>
        tidyr::pivot_wider(names_from = "name", values_from = "value") |> 
        dplyr::arrange("class_id") |> 
        dplyr::mutate(label = forcats::fct_inorder(.data$label)) |> 
        dplyr::rowwise() |> 
        dplyr::mutate(all_present = all(dplyr::c_across(dplyr::starts_with("att")) == 1)) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(prob = dplyr::case_when(.data$all_present ~ 1 - input$slip,
                                              !.data$all_present ~ input$guess))
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

dinoServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    probs <- reactive({
      num_att <- switch(input$atts,
                        One = 1L,
                        Two = 2L,
                        Three = 3L)
      
      profiles <- create_profiles(num_att) |> 
        tibble::rowid_to_column(var = "class_id") |>
        tidyr::pivot_longer(cols = -"class_id") |>
        dplyr::mutate(
          label = paste0("[", paste(.data$value, collapse = ","), "]"),
          .by = "class_id"
        ) |>
        tidyr::pivot_wider(names_from = "name", values_from = "value") |> 
        dplyr::arrange("class_id") |> 
        dplyr::mutate(label = forcats::fct_inorder(.data$label)) |> 
        dplyr::rowwise() |> 
        dplyr::mutate(all_present = any(dplyr::c_across(dplyr::starts_with("att")) == 1)) |> 
        dplyr::ungroup() |> 
        dplyr::mutate(prob = dplyr::case_when(.data$all_present ~ 1 - input$slip,
                                              !.data$all_present ~ input$guess))
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
