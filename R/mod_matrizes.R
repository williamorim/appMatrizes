#' matrizes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_matrizes_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Imagens como matrizes"),
    br(),
    fluidRow(
      class="row align-items-end",
      column(
        width = 2,
        actionButton(
          ns("transpor"),
          "Matriz transposta"
        )
      ),
      column(
        width = 2,
        actionButton(
          ns("multiplicar"),
          "Multiplicar por escalar"
        )
      ),
      column(
        width = 2,
        numericInput(
          ns("valor_escalar"),
          label = "Valor do escalar",
          value = 1,
          min = 0
        )
      ),
      column(
        offset = 1,
        width = 2,
        actionButton(
          ns("unir"),
          "C(t) = (1 - t)A + tB"
        )
      ),
      column(
        width = 2,
        numericInput(
          ns("valor_t"),
          label = "Valor de t [0, 1]",
          value = 0,
          min = 0,
          max = 1
        )
      )
    ),
    fluidRow(
      bs4Dash::bs4Card(
        width = 6,
        title = "Imagem original A",
        height = 550,
        fluidRow(
          column(
            offset = 1,
            width = 10,
            reactable::reactableOutput(ns("imagem_original_A"))
          )
        )
      ),
      bs4Dash::bs4Card(
        width = 6,
        height = 550,
        title = "Imagem transformada",
        fluidRow(
          column(
            offset = 1,
            width = 10,
            reactable::reactableOutput(ns("imagem_transformada"))
          )
        ),
        br(),
        fluidRow(
          class = "text-center",
          column(
            width = 12,
            actionButton(
              ns("voltar"),
              "Voltar para original"
            )
          )
        )
      )
    ),
    fluidRow(
      class = "text-center",
      bs4Dash::bs4Card(
        width = 6,
        title = "Imagem original B",
        height = 550,
        fluidRow(
          column(
            offset = 1,
            width = 10,
            reactable::reactableOutput(ns("imagem_original_B"))
          )
        )
      )
    )
  )
}

#' matrizes Server Functions
#'
#' @noRd
mod_matrizes_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    imagem_trans <- reactiveVal(1)

    im_original_A <- reactive({
      m <- readxl::read_excel("matriz.xlsx")
      imagem_trans(m)
      m
    })

    im_original_B <- reactive({
      readxl::read_excel("matriz.xlsx", sheet = 2)
    })

    output$imagem_original_A <- reactable::renderReactable({
      im_original_A() |>
        reactable::reactable(
          sortable = FALSE,
          borderless = TRUE,
          defaultColDef = reactable::colDef(
            name = "",
            width = 40,
            style = function(x) {
              list(backgroundColor = glue::glue("rgb({x}, {x}, {x})"))
            }
          )
        )
    })

    output$imagem_original_B <- reactable::renderReactable({
      im_original_B() |>
        reactable::reactable(
          sortable = FALSE,
          borderless = TRUE,
          defaultColDef = reactable::colDef(
            name = "",
            width = 40,
            style = function(x) {
              list(backgroundColor = glue::glue("rgb({x}, {x}, {x})"))
            }
          )
        )
    })

    output$imagem_transformada <- reactable::renderReactable({
      imagem_trans() |>
        reactable::reactable(
          sortable = FALSE,
          borderless = TRUE,
          defaultColDef = reactable::colDef(
            name = "",
            width = 40,
            style = function(x) {
              list(backgroundColor = glue::glue("rgb({x}, {x}, {x})"))
            },
            format = reactable::colFormat(
              digits = 0
            )
          )
        )
    })

    observeEvent(input$transpor, {
      m <- im_original_A() |>
        as.matrix() |>
        t() |>
        tibble::as_tibble()
      imagem_trans(m)
    })

    observeEvent(input$multiplicar, {
      m <- im_original_A() |>
        as.matrix()
      m <- m * input$valor_escalar
      m <- apply(m, c(1, 2), limitar_valor, maximo = 255)
      imagem_trans(tibble::as_tibble(m))
    })

    observeEvent(input$unir, {
      validate(need(
        input$valor_t >= 0 & input$valor_t <= 1,
        "t deve estar entre 0 e 1"
      ))
      mA <- im_original_A() |> as.matrix()
      mB <- im_original_B() |> as.matrix()
      matriz <- (1 - input$valor_t) * mA + input$valor_t * mB
      imagem_trans(tibble::as_tibble(matriz))
    })

    observeEvent(input$voltar, {
      imagem_trans(im_original_A())
    })


  })
}


