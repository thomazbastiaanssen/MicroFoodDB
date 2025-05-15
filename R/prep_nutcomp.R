library(tidyverse)

nutcomp <- read_csv(
    "raw/nutritional composition.csv",
    na = c("Tr", "", "NA", "N")
)


nutcomp_df <- nutcomp %>%
    pivot_longer(!1:7) %>%

    separate_wider_delim(
        cols = `Food Name`,
        delim = ", ",
        names = c("Food.Cat", "Rest"),
        too_few = "align_start",
        too_many = "merge",
        cols_remove = FALSE
    ) %>%
    dplyr::select(
        "Food.Cat",
        "Rest",
        Food.Name = `Food Name`,
        "name",
        "value"
    ) %>%

    filter(str_detect(
        Food.Name,
        pattern = "Broccoli, |Brussels|Cabbage, |Cauliflower, "
    )) %>%
    mutate(name = str_remove(name, " \\(g\\)")) %>%

    mutate(
        name = str_replace(name, "AOAC fibre", "Fiber, total dietary (AOAC)")
    ) %>%
    filter(name %in% c("Fiber, total dietary (AOAC)", "NSP", "Starch"))

rm(nutcomp)
