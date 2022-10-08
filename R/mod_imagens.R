#' imagens UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_imagens_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1("Transformando imagens preto e branco"),
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
        plotOutput(ns("imagem_original_A")),
        fileInput(
          ns("imagemA"),
          label = "Faça o upload da imagem A",
          buttonLabel = "Procurar...",
          placeholder = "Selecione um arquivo",
          accept = c(".png", ".jpeg", ".jpg")
        )
      ),
      bs4Dash::bs4Card(
        width = 6,
        height = 550,
        title = "Imagem transformada",
        plotOutput(ns("imagem_transformada")),
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
      bs4Dash::bs4Card(
        width = 6,
        title = "Imagem original B",
        height = 550,
        plotOutput(ns("imagem_original_B")),
        fileInput(
          ns("imagemB"),
          label = "Faça o upload da imagem B",
          buttonLabel = "Procurar...",
          placeholder = "Selecione um arquivo",
          accept = c(".jpeg", ".jpg")
        )
      )
    )
  )
}

#' imagens Server Functions
#'
#' @noRd
mod_imagens_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    imagem_trans <- reactiveVal(1)

    im_original_A <- reactive({
      if (!isTruthy(input$imagemA)) {
        im <- imager::load.image("inst/app/www/jake.jpeg") |>
          imager::grayscale()
        imagem_trans(im)
        im
      } else {
        im <- imager::load.image(input$imagemA$datapath) |>
          imager::grayscale() |>
          imager::resize(size_x = 512, size_y = 512)
        imagem_trans(im)
        im
      }
    })

    im_original_B <- reactive({
      if (!isTruthy(input$imagemB)) {
        im <- imager::load.image("inst/app/www/finn.jpeg") |>
          imager::resize(size_x = 512, size_y = 512) |>
          imager::grayscale()
        im
      } else {
        im <- imager::load.image(input$imagemB$datapath) |>
          imager::grayscale() |>
          imager::resize(size_x = 512, size_y = 512)
        im
      }
    })

    output$imagem_original_A <- renderPlot({
      plot(im_original_A(), rescale = FALSE)
    })

    output$imagem_original_B <- renderPlot({
      plot(im_original_B(), rescale = FALSE)
    })

    output$imagem_transformada <- renderPlot({
      plot(imagem_trans(), rescale = FALSE)
    })

    observeEvent(input$transpor, {
      dados_im <- as.array(im_original_A())
      matriz <- as.matrix(dados_im[,,1,1])
      dados_im[,,1,1] <- t(matriz)
      imagem_trans(imager::as.cimg(dados_im))
    })

    observeEvent(input$multiplicar, {
      dados_im <- as.array(im_original_A())
      matriz <- as.matrix(dados_im[,,1,1])
      matriz <- matriz * input$valor_escalar
      matriz <- apply(matriz, c(1, 2), limitar_valor)
      dados_im[,,1,1] <- matriz
      imagem_trans(imager::as.cimg(dados_im))
    })

    observeEvent(input$unir, {
      dados_im_A <- as.array(im_original_A())
      dados_im_B <- as.array(im_original_B())
      matriz_A <- as.matrix(dados_im_A[,,1,1])
      matriz_B <- as.matrix(dados_im_B[,,1,1])
      matriz <- (1 - input$valor_t) * matriz_A + input$valor_t * matriz_B
      dados_im_A[,,1,1] <- matriz
      imagem_trans(imager::as.cimg(dados_im_A))
    })

    observeEvent(input$voltar, {
      imagem_trans(im_original_A())
    })

  })
}

