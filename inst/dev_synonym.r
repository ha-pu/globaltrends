# load packages ----------------------------------------------------------------
library(DBI)
library(tidyverse)

devtools::load_all()

# create db --------------------------------------------------------------------
initialize_db()
start_db()

add_control_keyword(
    keyword = c("gmail", "map", "wikipedia", "youtube"),
    start_date = "2010-01",
    end_date = "2019-12"
)

add_object_keyword(
    keyword = list(
        c("fc barcelona", "fc bayern", "manchester united", "real madrid"),
        c("bayern munich", "bayern munchen", "manu", "real")
    ),
    start_date = "2010-01",
    end_date = "2019-12"
)

location_set <- c("US", "CN", "JP")

add_synonym(
    keyword = "fc bayern",
    synonym = c("bayern munich", "bayern munchen")
)
add_synonym(
    keyword = "manu",
    synonym = "manchester united"
)
add_synonym(
    keyword = "real madrid",
    synonym = "real"
)

data <- filter(example_control, batch == 1 & location %in% location_set[1:3])
dbAppendTable(gt.env$globaltrends_db, "data_control", data)
data <- filter(
    example_object,
    batch_c == 1 & batch_o == 1 & location %in% location_set[1:2]
) %>%
    mutate(batch_o = 1)
dbAppendTable(gt.env$globaltrends_db, "data_object", data)
data <- filter(
    example_object,
    batch_c == 1 & batch_o == 2 & location %in% location_set[2:3]
) %>%
    mutate(batch_o = 2)
dbAppendTable(gt.env$globaltrends_db, "data_object", data)

compute_score(object = 1:2)

# identify synonyms ------------------------------------------------------------
walk(
    location_set,
    ~ {
        lst_org_syn <- inner_join(
            gt.env$tbl_keywords,
            gt.env$tbl_synonyms,
            by = "keyword"
        )
        lst_syn_org <- inner_join(
            gt.env$tbl_synonyms,
            gt.env$tbl_keywords,
            by = c("synonym" = "keyword")
        )

        score_o <- filter(gt.env$tbl_score, location == .x)
        score_o <- semi_join(
            score_o,
            lst_org_syn,
            by = c("batch_o" = "batch", "keyword")
        )
        lst_org_syn <- semi_join(
            lst_org_syn,
            score_o,
            by = c("batch" = "batch_o")
        )
        lst_syn_org <- semi_join(
            lst_syn_org,
            lst_org_syn,
            by = c("keyword", "synonym")
        )

        score_s <- filter(gt.env$tbl_score, location == .x)
        score_s <- semi_join(
            score_s,
            lst_syn_org,
            by = c("batch_o" = "batch", "keyword" = "synonym")
        )
        lst_syn_org <- semi_join(
            lst_syn_org,
            score_s,
            by = c("synonym" = "keyword")
        )
        lst_org_syn <- semi_join(
            lst_org_syn,
            lst_syn_org,
            by = c("keyword", "synonym")
        )

        check <- count(score_o)
        check <- collect(check)
        if (check$n == 0) {
            return(NULL)
        }
        check <- count(score_s)
        check <- collect(check)
        if (check$n == 0) {
            return(NULL)
        }

        score_s <- lst_syn_org %>%
            inner_join(lst_org_syn, by = c("keyword", "synonym")) %>%
            select(
                batch_oo = batch.y,
                keyword_o = keyword,
                batch_os = batch.x,
                keyword_s = synonym
            ) %>%
            inner_join(
                score_s,
                by = c("batch_os" = "batch_o", "keyword_s" = "keyword")
            ) %>%
            summarise(
                score = sum(score, na.rm = TRUE),
                .by = c(batch_oo, keyword_o, location, date, batch_c)
            ) %>%
            rename(
                batch_o = batch_oo,
                keyword = keyword_o
            )

        df_score_new <- bind_rows(
            collect(score_o),
            collect(score_s)
        ) %>%
            summarise(
                score = sum(score, na.rm = TRUE),
                .by = c(batch_o, keyword, location, date, batch_c)
            )

        lst_keyword <- unique(c(
            collect(lst_org_syn)$keyword,
            collect(lst_org_syn)$synonym
        ))
        placeholders <- paste(rep("?", length(lst_keyword)), collapse = ", ")
        sql <- glue::glue(
            "DELETE FROM data_score WHERE location LIKE '{.x}' AND keyword IN ({placeholders})"
        )

        dbExecute(
            conn = gt.env$globaltrends_db,
            statement = sql,
            params = as.list(lst_keyword)
        )

        dbAppendTable(
            conn = gt.env$globaltrends_db,
            name = "data_score",
            value = df_score_new
        )
    }
)


disconnect_db()

# original function ------------------------------------------------------------

.aggregate_synonym <- function(object) {
    lst_synonym <- filter(gt.env$keywords_object, .data$batch == object)
    lst_synonym1 <- inner_join(
        lst_synonym,
        gt.env$keyword_synonyms,
        by = "keyword",
        multiple = "all"
    )
    lst_synonym2 <- inner_join(
        lst_synonym,
        gt.env$keyword_synonyms,
        by = c("keyword" = "synonym"),
        relationship = "many-to-one"
    )
    lst_synonym <- unique(c(lst_synonym1$synonym, lst_synonym2$keyword))

    if (length(lst_synonym) > 0) {
        message("Checking for synonyms...")
        data_synonym <- filter(
            gt.env$tbl_score,
            .data$keyword %in% lst_synonym & .data$synonym == 1
        )
        data_synonym <- collect(data_synonym)

        if (nrow(data_synonym) > 0) {
            message("Aggregating scores for synonyms...")
            lst_main <- unique(gt.env$keyword_synonyms$keyword[
                gt.env$keyword_synonyms$synonym %in% lst_synonym
            ])
            data_main <- filter(gt.env$tbl_score, .data$keyword %in% lst_main)
            data_main <- collect(data_main)

            walk(
                lst_synonym,
                ~ {
                    keyword_main <- gt.env$keyword_synonyms$keyword[
                        gt.env$keyword_synonyms$synonym == .x
                    ][[1]]
                    sub_main <- filter(data_main, .data$keyword == keyword_main)

                    sub_synonym <- filter(data_synonym, .data$keyword == .x)
                    sub_main <- left_join(
                        sub_main,
                        sub_synonym,
                        by = c("location", "date", "batch_c"),
                        suffix = c("", "_s"),
                        relationship = "many-to-one"
                    )

                    sub_main <- mutate(
                        sub_main,
                        score = .data$score + coalesce(.data$score_s, 0)
                    )
                    sub_main <- select(
                        sub_main,
                        location,
                        keyword,
                        date,
                        score,
                        batch_c,
                        batch_o,
                        synonym
                    )

                    data_synonym_agg <- inner_join(
                        sub_synonym,
                        select(
                            sub_main,
                            location,
                            date,
                            batch_c
                        ),
                        by = c("location", "date", "batch_c"),
                        relationship = "many-to-one"
                    )
                    data_synonym_agg <- mutate(data_synonym_agg, synonym = 2)
                    data_synonym_nagg <- anti_join(
                        sub_synonym,
                        select(
                            sub_main,
                            location,
                            date,
                            batch_c
                        ),
                        by = c("location", "date", "batch_c")
                    )

                    data <- bind_rows(
                        sub_main,
                        data_synonym_agg,
                        data_synonym_nagg
                    )
                    dbExecute(
                        conn = gt.env$globaltrends_db,
                        statement = "DELETE FROM data_score WHERE keyword=?",
                        params = list(keyword_main)
                    )
                    dbExecute(
                        conn = gt.env$globaltrends_db,
                        statement = "DELETE FROM data_score WHERE keyword=?",
                        params = list(.x)
                    )
                    dbAppendTable(
                        conn = gt.env$globaltrends_db,
                        name = "data_score",
                        value = data
                    )
                }
            )
        }
    }
}
