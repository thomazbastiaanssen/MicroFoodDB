library(tidyverse)

# Tr -> trace non-zero amount
# N  -> substantial, non-reliable measurement. non-zero.

tab <- read.delim("MicroFoodDB/raw/nutritional composition.csv", sep = ",")

tab_subset <- tab[, -c(1:6)]

ind <- str_detect(colnames(tab_subset), "\\.g.$")

tab_subset <- tab_subset[, ind]

tab_subset[tab_subset == ""] <- NA
tab_subset[tab_subset == "NA"] <- NA
tab_subset[tab_subset == "N"] <- NA
tab_subset[tab_subset == "Tr"] <- NA


tab_subset <- as.data.frame(
    lapply(tab_subset, as.numeric)
)

cbind(tab[, 1:6], tab_subset) %>%
    pivot_longer(!1:6) %>%
    mutate(
        Food.Name = str_replace(Food.Name, pattern = " ", replacement = "_"),
        Food.Name = str_replace(Food.Name, pattern = ",", replacement = "_")
    ) %>%

    separate_wider_delim(
        cols = Food.Name,
        delim = "_",
        names = c("Food.Cat", "Rest"),
        too_few = "align_start",
        too_many = "merge",
        cols_remove = FALSE
    ) %>%

    filter(name %in% c("Fat..g.", "NSP..g.")) %>%

    ggplot() +
    aes(x = Food.Name, y = value, fill = Food.Cat) +

    geom_boxplot(aes(group = Food.Cat)) +
    geom_point(shape = 21) +

    xlab(NULL) +
    ylab(NULL) +

    facet_wrap(~name) +
    theme_bw() +
    theme(axis.text.x = element_blank())
