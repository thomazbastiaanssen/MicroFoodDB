cabbage_df <- foodb.content %>%
    filter(source_type == "Compound") %>%
    dplyr::rename(
        "FDB" = "source_id",
        "FOOD" = "food_id",
        "food_name" = "orig_food_common_name",
        "value" = "standard_content",
        "source_id" = "orig_source_id"
    ) %>%
    dplyr::select(!c("created_at", "updated_at", "id")) %>%

    filter(
        str_detect(
            food_name,
            "Cabbage, chinese \\(pak-choi\\), |Cabbage, chinese \\(pe-tsai\\), |Cabbage, red, |Cabbage, savoy, ",
        )
    ) %>%

    filter(str_detect(food_name, "canned|pickled", negate = TRUE)) %>%

    mutate(
        FOOD = paste0("FOOD", formatC(FOOD, width = 5, flag = "0")),
        FDB = paste0("FDB", formatC(FDB, width = 6, flag = "0"))
    ) %>%

    #filter(FDB == "FDB001413")
    filter(!is.na(value)) %>%
    filter(source_id != "") %>%

    left_join(., FDB2name, by = "FDB") %>%
    filter(!is.na(name)) %>%
    group_by(orig_food_id, food_name) %>%
    mutate(FOOD = paste(unique(FOOD), collapse = "_")) %>%
    ungroup() %>%

    distinct() %>%

    #filter(str_detect(food_name, "Common ", negate = TRUE)) %>%
    #filter(str_detect(food_name, "Cabbage, ")) %>%

    # mutate(
    #     food_short = str_remove(food_name, "Cabbage, "),
    #     food_state = case_when(
    #         str_detect(string = food_short, "cooked") ~ "cooked",
    #         str_detect(string = food_short, "raw") ~ "raw"
    #     )
    # ) %>%
    # filter(!is.na(food_name)) %>%
    # separate_wider_delim(
    #     food_short,
    #     names = c("food_1", "food_2"),
    #     delim = ", ",
    #     too_few = "align_start",
    #     too_many = "merge",
    #     cols_remove = FALSE
    # ) %>%
    # filter(!is.na(food_1)) %>%
    # mutate(
    #     food_2 = case_when(
    #         food_1 == "raw" & is.na(food_2) ~ "raw",
    #         .default = food_2
    #     )
    # ) %>%
    #
    # filter(
    #     food_2 %in%
    #         c(
    #             "cooked, boiled, drained, with salt",
    #             "cooked, boiled, drained, without salt",
    #             "raw"
    #         )
    # ) %>%
    #
    # filter(
    #     food_1 %in%
    #         c("chinese (pak-choi)", "chinese (pe-tsai)", "red", "savoy")
    # ) %>%
    mutate(metab_id = paste(orig_source_name, source_id, sep = "_")) %>%

    dplyr::select("food_name", "metab_id", "value") %>%
    #filter(name == "Sucrose")
    #distinct() %>% dim
    #complete(., food_name, orig_source_name, fill = list(value = 0)) %>%

    pivot_wider(names_from = metab_id, values_from = value, values_fill = 0) %>%
    column_to_rownames("food_name")

cabbage_df <- cabbage_df[rowSums(cabbage_df) != 0, colSums(cabbage_df) != 0]

cabbage.exp <- deleuze::dclr(cabbage_df)

#Apply the base R principal component analysis function on our CLR-transformed data.
data.a.pca <- prcomp(cabbage.exp)

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

#Add relevant information from the metadata
pca$ID = rownames(cabbage.exp)
pca$Food = vapply(
    strsplit(rownames(cabbage.exp), split = ", "),
    function(x) {
        paste(x[1], x[2], sep = ", ")
    },
    FUN.VALUE = ""
)
pca$Preparation = case_when(
    str_detect(
        rownames(cabbage.exp),
        pattern = "cooked, boiled, drained, without salt"
    ) ~
        "boiled without salt",
    str_detect(
        rownames(cabbage.exp),
        pattern = "cooked, boiled, drained, with salt"
    ) ~
        "boiled with salt",
    str_detect(rownames(cabbage.exp), pattern = "raw") ~ "raw"
)
