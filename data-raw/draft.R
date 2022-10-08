im <- magick::image_read("inst/app/www/jake.jpeg")

dados <- im |> magick::image_data()
str(dados)

dados[3, 1, 1]




im_col <- magick::image_read("inst/app/www/jake_color.jpeg")

dados <- im |> magick::image_data()

dados[3, 300,]


im <- imager::load.image("inst/app/www/jake_color.jpeg")
dados <- imager::grayscale(im) |> as.array()
plot(imager::as.cimg(dados))

dados[,,1,1] <- dados[,1,1,1] * 0
plot(imager::as.cimg(dados), axes = FALSE)

matriz <- as.matrix(dados[,,1,1])
dados[,,1,1] <- t(matriz)
plot(imager::as.cimg(dados))

imager::load.image("inst/app/www/finn.jpeg") |>
  imager::grayscale() |>
  imager::resize(size_x = 512, size_y = 512) |>
  plot()


matriz <- readxl::read_xlsx("matriz.xlsx", col_names = FALSE)

matriz |> as.matrix() |> t()


matriz |>
  reactable::reactable(
    borderless = TRUE,
    defaultColDef = reactable::colDef(
      name = "",
      width = 40,
      style = function(x) {
        list(backgroundColor = glue::glue("rgb({x}, {x}, {x})"))
      }
    )
  )










