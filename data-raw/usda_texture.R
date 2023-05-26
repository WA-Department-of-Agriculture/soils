# This script prepares the USDA soil texture data for use in
# `washi_texture_triangle()`. The data are stored internally only.

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

usda_texture <- list(polygons = polygons, labels = labels)

usethis::use_data(usda_texture, internal = TRUE, overwrite = TRUE)
