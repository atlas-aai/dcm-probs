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
                  fluidRow(lcdmUI("lcdm")),
                  fluidRow(dinoaUI("dina", name = "Deterministic Input, Noisy \"And\" Gate (DINA) Response Probabilities")),
                  fluidRow(dinoaUI("dino", name = "Deterministic Input, Noisy \"Or\" Gate (DINO) Response Probabilities")))
  )
  
  server <- function(input, output, session) {
    lcdmServer("lcdm")
    dinaServer("dina")
    dinoServer("dino")
  }
  
  shinyApp(ui, server, ...)
}
