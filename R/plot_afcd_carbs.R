library(tidyverse)
library(readxl)


afcd <- read_xlsx("raw/AFCD/Release 2 - Nutrient file.xlsx", sheet = 2)

afcd_scaled <- afcd %>% View
dplyr::select(c(1:3, 7:11, 12:40, 41:51)) %>%
    pivot_longer(!c(1:3)) %>%

    separate_wider_delim(
        name,
        names = c("name", "units"),
        delim = "\r\n",
        too_few = "align_start"
    ) %>%
    mutate(
        units = case_when(
            str_detect(name, "\\(g\\)") & is.na(units) ~ "(g)",
            .default = units
        )
    ) %>%
    mutate(name = str_remove(name, pattern = "\\(g\\)")) %>%
    mutate(name = str_remove(name, pattern = " $")) %>%
    mutate(name = str_remove(name, pattern = " $")) %>%
    mutate(
        type = case_when(
            name %in% c("Protein", "Nitrogen", "Fat, total") ~ "overall",
            str_detect(name, pattern = "Available carbohydrate") ~ "summary",
            str_detect(name, pattern = " sugars") ~ "summary",
            .default = "carb"
        )
    ) %>%

    filter(str_detect(Classification, "^121|^16|^22|^24|^251")) %>%
    mutate(
        food_type = case_when(
            str_detect(Classification, "^121") ~ "Flours, grains and starches",
            str_detect(Classification, "^16") ~ "Fruits",
            str_detect(Classification, "^22") ~ "Nuts",
            str_detect(Classification, "^24") ~ "Vegetables",
            str_detect(Classification, "^251") ~ "Legumes and pulses",
            .default = "ERROR"
        )
    ) %>%
    mutate(value = replace_na(value, 0)) %>%

    group_by(food_type, name) %>%
    filter(max(value) != 0) %>%
    ungroup() %>%

    filter(type == "carb") %>%
    complete(., name, `Food Name`, fill = list(value = 0)) %>%
    mutate(value = log1p(value)) %>%

    group_by(name) %>%
    mutate(value = value - mean(value)) %>%
    ungroup() %>%

    group_by(`Food Name`) %>%
    mutate(value = value - mean(value)) %>%
    ungroup()

data.a.pca <- afcd_scaled %>%
    dplyr::select("Food Name", "name", "value") %>%
    pivot_wider(names_from = "name", values_from = "value") %>%
    column_to_rownames("Food Name") %>%
    t() %>%

    #Apply the base R principal component analysis function on our CLR-transformed data.
    prcomp()

#Extract the amount of variance the first four components explain for plotting.
pc1 <- round(data.a.pca$sdev[1]^2 / sum(data.a.pca$sdev^2), 4) * 100
pc2 <- round(data.a.pca$sdev[2]^2 / sum(data.a.pca$sdev^2), 4) * 100
pc3 <- round(data.a.pca$sdev[3]^2 / sum(data.a.pca$sdev^2), 4) * 100
pc4 <- round(data.a.pca$sdev[4]^2 / sum(data.a.pca$sdev^2), 4) * 100

#Extract the scores for every sample for the first four components for plotting.
pca = data.frame(
    PC1 = data.a.pca$x[, 1],
    PC2 = data.a.pca$x[, 2],
    PC3 = data.a.pca$x[, 3],
    PC4 = data.a.pca$x[, 4]
)


pca %>%
    rownames_to_column("name") %>%

    left_join(., afcd_scaled, by = "name") %>%

    ggplot() +

    aes(x = PC1, y = PC2, fill = name) +
    geom_point(shape = 21) +

    theme_bw()

pca %>%
    rownames_to_column("Food Name") %>%

    left_join(., afcd_scaled, by = "Food Name") %>%

    ggplot() +

    aes(x = PC1, y = PC2, fill = food_type) +
    geom_point(shape = 21) +

    theme_bw()

#filter(food_type == "Flours, grains and starches") %>%

# filter(food_type == "Legumes and pulses") %>%
# filter(food_type == "Nuts") %>%
# filter(food_type == "Fruits") %>%
# filter(food_type == "Vegetables") %>%

ggplot() +

    aes(x = name, y = `Food Name`, fill = value, label = value) +
    geom_tile() +
    # geom_text(colour = "white") +

    scale_y_discrete(position = "right") +
    scale_fill_gradient2(
        "Content\n(scaled)",
        high = "red",
        low = "blue",
        midpoint = 0
    ) +

    facet_wrap(~food_type, scales = "free_y", nrow = 1) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 345, hjust = 0))
