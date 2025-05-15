library(tidyverse)
library(readxl)


afcd <- read_xlsx("raw/AFCD/Release 2 - Nutrient file.xlsx", sheet = 2)

afcd_df <- afcd %>%
    dplyr::select(
        Food.Name = "Food Name",
        "Total dietary fibre \r\n(g)",
        "Starch \r\n(g)"
    ) %>%
    filter(str_detect(
        Food.Name,
        pattern = "Broccoli|Brussels|Cabbage|Cauliflower"
    )) %>%
    filter(str_detect(Food.Name, pattern = "Broccolini", negate = TRUE)) %>%
    pivot_longer(!Food.Name) %>%

    mutate(name = str_remove(name, " \\r\\n\\(g\\)$")) %>%

    mutate(
        name = str_replace(name, "Total dietary fibre", "Fiber, total dietary")
    ) %>%

    separate_wider_delim(
        cols = Food.Name,
        delim = ", ",
        names = c("Food.Cat", "Rest"),
        too_few = "align_start",
        too_many = "merge",
        cols_remove = FALSE
    ) %>%
    mutate(
        Food.Cat = str_replace(Food.Cat, "Brussels sprout$", "Brussels sprouts")
    )

rm(afcd)
#
# afcd_df %>%
#
#     ggplot() +
#     aes(x = Food.Cat, y = value, fill = Food.Cat) +
#
#     geom_boxplot(aes(group = Food.Cat)) +
#     geom_point(shape = 21) +
#
#     xlab(NULL) +
#     ylab(NULL) +
#
#     facet_wrap(~name) +
#     theme_bw() +
#     theme(axis.text.x = element_blank())
