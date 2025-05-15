library(tidyverse)
library(readxl)


foodb <- read.delim("raw/foodb_2020_4_7_csv/Content.csv", sep = ",")


foodb_df <- foodb %>%
    dplyr::rename(Food.Name = orig_food_common_name) %>%
    filter(
        orig_source_name %in%
            c(
                "Fiber, dietary",
                "Fiber, total dietary (AOAC)",
                "Fiber, total dietary"
            )
    ) %>%
    filter(str_detect(
        Food.Name,
        pattern = "Broccoli|Brussels|Cabbage|Cauliflower"
    )) %>%
    filter(str_detect(
        Food.Name,
        pattern = "Broccoli raab",
        negate = TRUE
    )) %>%
    filter(str_detect(
        Food.Name,
        pattern = "CAMPBELL Soup Company|HOT POCKETS",
        negate = TRUE
    )) %>%

    separate_wider_delim(
        cols = Food.Name,
        delim = ", ",
        names = c("Food.Cat", "Rest"),
        too_few = "align_start",
        too_many = "merge",
        cols_remove = FALSE
    ) %>%
    mutate(
        Food.Cat = str_replace(
            Food.Cat,
            pattern = "Brussels sprout$",
            replacement = "Brussels sprouts"
        )
    ) %>%

    dplyr::select(
        Food.Name,
        Food.Cat,
        Rest,
        value = standard_content,
        name = orig_source_name
    )

rm(foodb)
#
# foodb_df %>%
#
#     ggplot() +
#     aes(x = Food.Cat, y = value, fill = Food.Cat) +
#
#     geom_boxplot(aes(group = Food.Cat)) +
#     geom_point(shape = 21) +
#
#     xlab(NULL) +
#     ylab("mg/100g") +
#
#     facet_wrap(~name) +
#     theme_bw() +
#     theme(axis.text.x = element_blank())
