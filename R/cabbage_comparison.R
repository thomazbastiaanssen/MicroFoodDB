source("R/foodb_to_linkmaps.R")


# Link compounds to foods
FDB2FOOD <- foodb.content %>%
    filter(source_type == "Compound") %>%
    dplyr::rename("FDB" = "source_id", "FOOD" = "food_id") %>%
    dplyr::select(!c("orig_food_scientific_name", "orig_food_part")) %>%

    #  dplyr::select("orig_max", "orig_min", "orig_content")

    mutate(
        FOOD = paste0("FOOD", formatC(FOOD, width = 5, flag = "0")),
        FDB = paste0("FDB", formatC(FDB, width = 6, flag = "0"))
    )

FDB2FOOD %>%
    filter(!is.na(orig_max) & !is.na(orig_min) & !is.na(orig_content))

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

    filter(str_detect(food_name, "abbage")) %>%
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

    filter(str_detect(food_name, "Common ", negate = TRUE)) %>%
    filter(str_detect(food_name, "Cabbage, ")) %>%

    mutate(
        food_short = str_remove(food_name, "Cabbage, "),
        food_state = case_when(
            str_detect(string = food_short, "cooked") ~ "cooked",
            str_detect(string = food_short, "raw") ~ "raw"
        )
    ) %>%
    filter(!is.na(food_name)) %>%
    separate_wider_delim(
        food_short,
        names = c("food_1", "food_2"),
        delim = ", ",
        too_few = "align_start",
        too_many = "merge",
        cols_remove = FALSE
    ) %>%
    filter(!is.na(food_1)) %>%
    mutate(
        food_2 = case_when(
            food_1 == "raw" & is.na(food_2) ~ "raw",
            .default = food_2
        )
    ) %>%

    filter(
        food_2 %in%
            c(
                "cooked, boiled, drained, with salt",
                "cooked, boiled, drained, without salt",
                "raw"
            )
    ) %>%

    filter(
        food_1 %in%
            c("chinese (pak-choi)", "chinese, (pe-tsai)", "red", "savoy")
    ) %>%

    mutate(value = log(value)) %>%

    group_by(name) %>%
    mutate(value = scale(value)[,]) %>%
    ungroup() %>%

    mutate(
        value = replace_na(value, 0),
        value = replace(value, is.nan(value), 0)
    ) %>%

    group_by(food_1, food_2) %>%
    mutate(keep = max(value) > 0) %>%
    ungroup() %>%
    filter(keep) %>%

    group_by(food_name) %>%
    mutate(value = scale(value)[,]) %>%
    ungroup() %>%
    dplyr::select(!keep) %>%

    dplyr::select(food_name, name, value, food_1, food_2) %>%

    complete(., food_name, name, fill = list(value = 0)) %>%
    mutate(
        value = replace_na(value, 0),
        value = replace(value, is.nan(value), 0)
    ) #%>%
# group_by(food_1, food_2) %>%
# mutate(keep = max(value) != 0) %>%
# ungroup() %>%
#filter(keep)

cabbage_df %>%
    filter(!is.na(food_1)) %>%

    # dplyr::select(food_2, food_1) %>%
    # distinct() %>%
    # table() %>% rowSums()

    ggplot() +

    aes(x = food_2, y = name, fill = value) +
    geom_tile() +

    scale_y_discrete(position = "right") +
    scale_fill_gradient2(high = "red", low = "blue", midpoint = 0) +

    facet_wrap(~food_1, scales = "free_x") +
    theme(axis.text.x = element_text(hjust = 0, angle = 330))
