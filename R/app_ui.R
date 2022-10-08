#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bs4Dash::dashboardPage(
      dark = NULL,
      bs4Dash::bs4DashNavbar(),
      bs4Dash::bs4DashSidebar(
        bs4Dash::bs4SidebarMenu(
          bs4Dash::bs4SidebarMenuItem(
            "Com matrizes",
            tabName = "matrizes"
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Com imagens",
            tabName = "imagens"
          )
        )
      ),
      bs4Dash::bs4DashBody(
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "matrizes",
            mod_matrizes_ui("matrizes_1")
          ),
          bs4Dash::bs4TabItem(
            tabName = "imagens",
            mod_imagens_ui("imagens_1")
          )
        )
      ),
      footer = bs4Dash::bs4DashFooter(
        left = p(
          "Desenvolvido por", a(
            href = "https://www.instagram.com/gtaua/",
            target = "_blank",
            HTML("Taua Gomes"),
          ),
          "e",
          a(
            href = "https://twitter.com/wamorim_",
            target = "_blank",
            HTML("William Amorim"),
          ),
          "com base",
          a(
            href = "https://www.youtube.com/watch?v=cZO7tLRzEgQ&list=PLELvEtpsTDQQ0omPPEyVHgceDN85As0Jj",
            target = "_blank",
            "neste vídeo.",
          )
        ),
        right = p(
          "Feito em",
          a(href = "https://shiny.rstudio.com/", "Shiny", target = "_blank")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "appMatrizes"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
