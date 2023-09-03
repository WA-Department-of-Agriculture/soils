# This script prepares the USDA soil texture data for use in
# `make_texture_triangle()`.

data(USDA, package = "ggtern")

colnames(USDA) <- tolower(colnames(USDA))

polygons <- USDA

labels <- USDA |>
  dplyr::summarize(
    clay = mean(clay),
    sand = mean(sand),
    silt = mean(silt),
    .by = label
  ) |>
  dplyr::mutate(
    angle =
      dplyr::case_when(
        label == "Loamy Sand" ~ -35,
        TRUE ~ 0
      )
  )

usdaTexture <- list(polygons = polygons, labels = labels)

usethis::use_data(usdaTexture, internal = TRUE, overwrite = TRUE)
